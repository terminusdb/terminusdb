use std::collections::VecDeque;
use std::sync::{Arc, Weak, RwLock};
use swipl::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;
use std::io::Write;

predicates! {
    #[module("$exclusion")]
    semidet fn new_exclusions(_context, width_term, height_term, exclusions_term) {
        let width: u64 = width_term.get()?;
        let height: u64 = height_term.get()?;
        exclusions_term.unify(Exclusions::new(width as usize, height as usize))
    }

    #[module("$exclusion")]
    semidet fn generate_provisionals(_context, exclusions_term, width_term, height_term, provisionals_list_term, provisionals_term) {
        let exclusions: Exclusions = exclusions_term.get()?;
        let width: usize = width_term.get::<u64>()?.try_into().unwrap();
        let height: usize = height_term.get::<u64>()?.try_into().unwrap();

        let (provisionals_list, provisionals) = exclusions.generate_provisionals(width, height);

        provisionals_list_term.unify(provisionals_list.as_slice())?;
        provisionals_term.unify(provisionals)
    }

    #[module("$exclusion")]
    semidet fn select_provisional(_context, provisional_term) {
        let provisional: ProvisionalExclusion = provisional_term.get()?;
        provisional.select();

        Ok(())
    }

    #[module("$exclusion")]
    semidet fn integrate_provisionals(_context, exclusions_term, provisionals_term) {
        let exclusions: Exclusions = exclusions_term.get()?;
        let provisionals: ProvisionalExclusions = provisionals_term.get()?;

        exclusions.integrate_provisionals(&provisionals);

        Ok(())
    }

    #[module("$exclusion")]
    semidet fn provisional_valid(_context, provisional_term) {
        let provisional: ProvisionalExclusion = provisional_term.get()?;

        into_prolog_result(provisional.valid())
    }

    #[module("$exclusion")]
    semidet fn provisional_selected(_context, provisional_term) {
        let provisional: ProvisionalExclusion = provisional_term.get()?;

        into_prolog_result(provisional.selected())
    }

    #[module("$exclusion")]
    semidet fn exclusions_as_list(_context, exclusions_term, list_term) {
        let exclusions: Exclusions = exclusions_term.get()?;

        let guard = exclusions.vec.read().unwrap();
        list_term.unify(guard.as_slice())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ExclusionState {
    Unselected,
    Selected,
    Invalid,
}

#[derive(Clone)]
#[clone_blob("exclusion")]
struct Exclusion {
    left_x: usize,
    top_y: usize,
    right_x: usize,
    bottom_y: usize,
}

impl CloneBlobImpl for Exclusion {
    fn write(&self, stream: &mut PrologStream) -> std::io::Result<()> {
        write!(stream, "<exclusion {}x{}+{}*{}>",
               self.left_x,
               self.top_y,
               self.right_x-self.left_x+1,
               self.bottom_y-self.top_y+1)
    }
}

impl From<ProvisionalExclusion> for Exclusion {
    fn from(e: ProvisionalExclusion) -> Self {
        Self {
            left_x: e.left_x,
            top_y: e.top_y,
            right_x: e.right_x,
            bottom_y: e.bottom_y
        }
    }
}

#[derive(Clone)]
#[clone_blob("provisional_exclusion")]
pub struct ProvisionalExclusion {
    left_x: usize,
    top_y: usize,
    right_x: usize,
    bottom_y: usize,

    /// is this a valid exclusion? This starts out as true but is marked false when one of the overlapping ones is marked as true.
    state: Arc<RwLock<ExclusionState>>,

    /// All the exclusions that overlap with this exclusion.
    overlaps: Vec<Weak<RwLock<ExclusionState>>>,

    /// A reference into the structure where we can register our exclusion
    exclusions: ProvisionalExclusions
}

impl CloneBlobImpl for ProvisionalExclusion {
    fn write(&self, stream: &mut PrologStream) -> std::io::Result<()> {
        write!(stream, "<provisional_exclusion {}x{}+{}x{} ({:?}, {})>",
               self.left_x,
               self.top_y,
               self.right_x-self.left_x+1,
               self.bottom_y-self.top_y+1,
               *self.state.read().unwrap(),
               self.overlaps.len())
    }
}

impl ProvisionalExclusion {
    fn new(exclusions: ProvisionalExclusions, overlaps: Vec<Weak<RwLock<ExclusionState>>>, state: Arc<RwLock<ExclusionState>>, left_x: usize, top_y: usize, right_x: usize, bottom_y: usize) -> Self {
        Self {
            left_x,
            top_y,
            right_x,
            bottom_y,
            exclusions,

            overlaps,
            state,
        }
    }
    pub fn valid(&self) -> bool {
        *self.state.read().unwrap() != ExclusionState::Invalid
    }

    pub fn selected(&self) -> bool {
        *self.state.read().unwrap() == ExclusionState::Selected
    }

    fn as_exclusion(&self) -> Exclusion {
        Exclusion {
            left_x: self.left_x,
            top_y: self.top_y,
            right_x: self.right_x,
            bottom_y: self.bottom_y,
        }
    }

    pub fn select(&self) {
        let state = self.state.read().unwrap();
        if *state == ExclusionState::Invalid {
            panic!("tried to select invalid exclusion");
        }
        else if *state == ExclusionState::Selected {
            // already done!
            return;
        }
        // dropping state here again to release the write lock.
        // we do this cause otherwise we'll get in a hold and wait
        // situation, and therefore might introduce a deadlock.
        std::mem::drop(state);

        for overlap in self.overlaps.iter() {
            let overlap = overlap.upgrade()
                .expect("overlapping window deallocated");
            let mut guard = overlap.write().expect("unlock for write failed");

            if *guard == ExclusionState::Selected {
                panic!("found an exclusion zone to be marked as invalid that is already selected");
            }
            *guard = ExclusionState::Invalid;
        }

        let mut state = self.state.write().unwrap();
        *state = ExclusionState::Selected;
        std::mem::drop(state);

        self.exclusions.add(self.as_exclusion());
    }
}

#[derive(Clone)]
#[clone_blob("exclusions", defaults)]
pub struct Exclusions {
    width: usize,
    height: usize,
    vec: Arc<RwLock<Vec<Exclusion>>>
}

impl Exclusions {
    pub fn new(width: usize, height: usize) -> Self {
        Self {
            width,
            height,
            vec: Default::default()
        }
    }

    pub fn integrate_provisionals(&self, provisionals: &ProvisionalExclusions) {
        // first we invalidate the provisional exclusions
        {
            let mut validity = provisionals.valid.write().unwrap();
            *validity = false;
        }

        // then we extract the extensions
        let mut extension = Vec::with_capacity(0);
        {
            let mut v = provisionals.vec.write().unwrap();
            std::mem::swap(&mut extension, &mut v);
        }

        let mut vec = self.vec.write().unwrap();
        vec.extend(extension);
        vec.sort_by_key(|e| e.top_y);
    }

    pub fn generate_provisionals(&self, width:usize, height:usize) -> (Vec<ProvisionalExclusion>, ProvisionalExclusions) {
        let orig = self.vec.read().unwrap();
        let mut exclusions = orig.clone();
        let mut exclusions_slice = exclusions.as_mut_slice();

        let provisionals = ProvisionalExclusions::new();
        let mut result = Vec::new();
        let mut past: VecDeque<Vec<Rc<RefCell<ProvisionalExclusion>>>> = VecDeque::with_capacity(height);

        for r in 0..self.height-height+1 {
            let mut past_row = Vec::new();
            let mut past_row_deque: VecDeque<Rc<RefCell<ProvisionalExclusion>>> = VecDeque::new();
            let mut past_indexes: Vec<usize> = vec![0;past.len()];

            // get a nice subvec
            let pos = match exclusions_slice.iter().position(|e| e.top_y > r + height-1) {
                Some(pos) => pos,
                None => exclusions_slice.len()
            };
            let row_exclusions = &mut exclusions_slice[0..pos];

            // row_exclusions contains all the exclusions that will
            // match something on this row. We now sort by left_x so
            // we get them in order.
            row_exclusions.sort_by_key(|e|e.left_x);

            // set up a little closure we'll be using multiple time.
            // This will create a new provisional exclusion for the
            // given x, and register all overlaps.
            let mut add_provisional = |x:usize| {
                let state = Arc::new(RwLock::new(ExclusionState::Unselected));
                let mut overlaps = Vec::new();

                // let's find all the overlaps in past rows, moving the start index as we search and encounter things we moved past.
                for (row_index, index) in past_indexes.iter_mut().enumerate() {
                    let rr = &past[row_index];
                    for col in *index..rr.len() {
                        let mut elt = rr[col].borrow_mut();
                        if elt.right_x < x {
                            // we moved past this guy. raising index so we don't consider it in the future.
                            *index += 1;
                        }
                        else if elt.left_x > x+width-1 {
                            // this guy is still ahead of us, as is everything else. We're done.
                            break;
                        }
                        else {
                            // this guy overlaps. let's register.
                            elt.overlaps.push(Arc::downgrade(&state));
                            overlaps.push(Arc::downgrade(&elt.state));
                        }
                    }
                }

                // clean the row deque of any element we moved past
                loop {
                    if let Some(elt) = past_row_deque.front().cloned() {
                        let elt = elt.borrow();
                        if elt.right_x < x {
                            // we moved past.
                            past_row_deque.pop_front();
                        }
                        else {
                            // this is still in scope
                            break;
                        }
                    }
                    else {
                        // nothing left
                        break;
                    }
                }

                for elt in past_row_deque.iter() {
                    // everything that remains on the deque is in scope
                    let mut elt = elt.borrow_mut();
                    elt.overlaps.push(Arc::downgrade(&state));
                    overlaps.push(Arc::downgrade(&elt.state));
                }

                let provisional = Rc::new(RefCell::new(ProvisionalExclusion::new(provisionals.clone(), overlaps, state.clone(), x, r, x+width-1, r+height-1)));
                result.push(provisional.clone());
                past_row.push(provisional.clone());
                past_row_deque.push_back(provisional);
            };

            // reserve a new vec to save row exclusions that we're keeping
            let mut new_row_exclusions = Vec::with_capacity(row_exclusions.len());
            let mut c = 0;
            let row_exclusions_len = row_exclusions.len();
            for exclusion in row_exclusions {
                if exclusion.left_x >= c + width {
                    // matches are possible

                    let right_bound = exclusion.left_x-width+1;
                    for x in c..right_bound {
                        add_provisional(x);
                    }
                }

                c = exclusion.right_x + 1;
                if exclusion.bottom_y != r {
                    // we will need this exclusion for the next row too
                    new_row_exclusions.push(exclusion.clone());
                }
                if c > self.width-width {
                    break;
                }
            }

            // there may be windows right of the last match
            for x in c..self.width-width+1 {
                add_provisional(x);
            }

            // glue exclusions back together
            new_row_exclusions.sort_by_key(|e|e.top_y);
            let offset = row_exclusions_len - new_row_exclusions.len();
            exclusions_slice = &mut exclusions_slice[offset..];
            exclusions_slice[0..new_row_exclusions.len()]
                .clone_from_slice(new_row_exclusions.as_slice());

            // update the past
            past.push_back(past_row);

            // only keep as many rows as we need for height-1
            if past.len() == height {
                past.pop_front();
            }
        }

        // clear the past, ensuring that the only remaining Rc's are contained in result.
        past.clear();

        (result.into_iter().map(|c| {
            match Rc::try_unwrap(c) {
                Ok(c) => c.into_inner(),
                Err(_) => panic!("rc unexpectedly had more than one strong reference remaining")
            }
        }).collect(),
         provisionals)
    }
}

/// This is the shared state carried around by exclusions.
#[derive(Clone)]
#[clone_blob("provisional_exclusions", defaults)]
pub struct ProvisionalExclusions {
    valid: Arc<RwLock<bool>>,
    vec: Arc<RwLock<Vec<Exclusion>>>
}

impl ProvisionalExclusions {
    fn new() -> Self {
        Self {
            valid: Arc::new(RwLock::new(true)),
            vec: Default::default()
        }
    }

    fn add(&self, exclusion: Exclusion) {
        if !*self.valid.read().unwrap() {
            panic!("tried to add exclusion to a provisional exclusions collection which has already been consumed");
        }

        let mut vec = self.vec.write().unwrap();
        vec.push(exclusion);
    }
}

pub fn register() {
    register_new_exclusions();
    register_generate_provisionals();
    register_integrate_provisionals();
    register_select_provisional();
    register_provisional_valid();
    register_provisional_selected();
    register_exclusions_as_list();
}
