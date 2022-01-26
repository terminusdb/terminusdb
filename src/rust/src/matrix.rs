use swipl::prelude::*;
use std::sync::{Arc, RwLock};
use std::io::Write;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::cmp::Ordering;

predicates! {
    #[module("$matrix")]
    semidet fn as_matrix(context, list_of_lists_term, matrix_term) {
        let mut col_len: Option<usize> = None;
        let mut data = Vec::new();
        for term in context.term_list_iter(list_of_lists_term) {
            let v_opt = attempt_opt(term.get())?;
            if v_opt.is_none() {
                let error_term = term!{context: error(not_a_list_of_atoms(#term), _)}?;
                return context.raise_exception(&error_term);
            }

            let v: Vec<Atom> = v_opt.unwrap();
            let current_col_len = v.len();
            if let Some(col_len) = col_len {
                if current_col_len != col_len {
                    let error_term = term!{context: error(inconsistent_column_length(#col_len as u64, #v.len() as u64), _)}?;
                    return context.raise_exception(&error_term);
                }
            }
            else if current_col_len == 0 {
                let error_term = term!{context: error(zero_length_row, _)}?;
                return context.raise_exception(&error_term);
            }
            else {
                col_len = Some(current_col_len);
            }

            data.extend(v);
        }

        if data.len() == 0 {
            let error_term = term!{context: error(no_rows, _)}?;
            return context.raise_exception(&error_term);
        }

        let row_len = data.len()/col_len.unwrap();

        data.shrink_to_fit();

        let matrix = Matrix {
            data,
            rows: row_len,
            cols: col_len.unwrap()
        };

        matrix_term.unify(Arc::new(matrix))
    }

    #[module("$matrix")]
    semidet fn as_list_of_lists(context, matrix_term, list_of_lists_term) {
        if let Some(matrix) = attempt_opt::<Arc<Matrix>>(matrix_term.get())? {
            matrix.as_list_of_lists(context, list_of_lists_term)
        }
        else if let Some(window) = attempt_opt::<Arc<Window>>(matrix_term.get())? {
            window.as_list_of_lists(context, list_of_lists_term)
        }
        else {
            let error_term = term!{context: error(type_error(one_of(matrix, window), #matrix_term), _)}?;
            context.raise_exception(&error_term)
        }
    }

    #[module("$matrix")]
    semidet fn matrix_window(context, matrix_term, x_term, y_term, width_term, height_term, window_term) {
        let matrix: Arc<Matrix> = matrix_term.get_ex()?;

        let x: u64 = x_term.get_ex()?;
        let y: u64 = y_term.get_ex()?;

        let width: u64 = width_term.get_ex()?;
        let height: u64 = height_term.get_ex()?;

        let window = context.try_or_die(
            matrix.window(x as usize,
                          y as usize,
                          width as usize,
                          height as usize))?;

        window_term.unify(Arc::new(window))
    }

    #[module("$matrix")]
    semidet fn window_window(context, orig_window_term, x_term, y_term, width_term, height_term, window_term) {
        let window: Arc<Window> = orig_window_term.get_ex()?;

        let x: u64 = x_term.get_ex()?;
        let y: u64 = y_term.get_ex()?;

        let width: u64 = width_term.get_ex()?;
        let height: u64 = height_term.get_ex()?;

        let new_window = context.try_or_die(
            window.window(x as usize,
                          y as usize,
                          width as usize,
                          height as usize))?;

        window_term.unify(Arc::new(new_window))
    }

    #[module("$matrix")]
    semidet fn matrix_row(context, matrix_term, row_num_term, row_term) {
        let matrix: Arc<Matrix> = matrix_term.get_ex()?;
        let row_num: usize = row_num_term.get_ex::<u64>()? as usize;
        let row = context.try_or_die(matrix.row(row_num))?;

        row_term.unify(row)
    }

    #[module("$matrix")]
    semidet fn matrix_col(context, matrix_term, col_num_term, col_term) {
        let matrix: Arc<Matrix> = matrix_term.get_ex()?;
        let col_num: usize = col_num_term.get_ex::<u64>()? as usize;
        let col = context.try_or_die(matrix.col(col_num))?;

        col_term.unify(col.as_slice())
    }

    #[module("$matrix")]
    semidet fn window_row(context, window_term, row_num_term, row_term) {
        let window: Arc<Window> = window_term.get_ex()?;
        let row_num: usize = row_num_term.get_ex::<u64>()? as usize;
        let row = context.try_or_die(window.row(row_num))?;

        row_term.unify(row)
    }

    #[module("$matrix")]
    semidet fn window_col(context, window_term, col_num_term, col_term) {
        let window: Arc<Window> = window_term.get_ex()?;
        let col_num: usize = col_num_term.get_ex::<u64>()? as usize;
        let col = context.try_or_die(window.col(col_num))?;

        col_term.unify(col.as_slice())
    }
}

#[arc_blob("matrix")]
pub struct Matrix {
    data: Vec<Atom>,
    rows: usize,
    cols: usize
}

impl Matrix {
    fn window(self: &Arc<Self>, x: usize, y: usize, width: usize, height: usize) -> Result<Window, WindowError> {
        if width == 0 {
            Err(WindowError::WidthZero)
        }
        else if height == 0 {
            Err(WindowError::HeightZero)
        }
        else if x >= self.cols {
            Err(WindowError::XOutOfRange)
        }
        else if y >= self.rows {
            Err(WindowError::YOutOfRange)
        }
        else if x+width > self.cols {
            Err(WindowError::WidthOutOfRange)
        }
        else if y+height > self.rows {
            Err(WindowError::HeightOutOfRange)
        }
        else {
            Ok(Window {
                matrix: self.clone(),
                x,
                y,
                width,
                height,
                cached_hash: Default::default()
            })
        }
    }
}

enum WindowError {
    XOutOfRange,
    YOutOfRange,
    WidthZero,
    HeightZero,
    WidthOutOfRange,
    HeightOutOfRange
}

impl IntoPrologException for WindowError {
    fn into_prolog_exception<'a, 'b, T: QueryableContextType>(
        self,
        context: &'a Context<'b, T>) -> PrologResult<Term<'a>> {
        match self {
            WindowError::XOutOfRange => term!{context: error(x_out_of_range, _)},
            WindowError::YOutOfRange => term!{context: error(y_out_of_range, _)},
            WindowError::WidthZero => term!{context: error(width_zero, _)},
            WindowError::HeightZero => term!{context: error(height_zero, _)},
            WindowError::WidthOutOfRange => term!{context: error(width_out_of_range, _)},
            WindowError::HeightOutOfRange => term!{context: error(height_out_of_range, _)},
        }
    }
}

impl ArcBlobImpl for Matrix {
    fn write(&self, stream: &mut PrologStream) -> std::io::Result<()> {
        write!(stream, "<matrix {}x{}>", self.cols, self.rows)
    }
}

#[arc_blob("window")]
pub struct Window {
    matrix: Arc<Matrix>,
    x: usize,
    y: usize,
    width: usize,
    height: usize,

    cached_hash: Arc<RwLock<Option<u64>>>
}

impl Window {
    fn window(self: &Arc<Self>, x: usize, y: usize, width: usize, height: usize) -> Result<Window, WindowError> {
        if width == 0 {
            Err(WindowError::WidthZero)
        }
        else if height == 0 {
            Err(WindowError::HeightZero)
        }
        else if x >= self.width {
            Err(WindowError::XOutOfRange)
        }
        else if y >= self.height {
            Err(WindowError::YOutOfRange)
        }
        else if x+width > self.width {
            Err(WindowError::WidthOutOfRange)
        }
        else if y+height > self.height {
            Err(WindowError::HeightOutOfRange)
        }
        else {
            Ok(Window {
                matrix: self.matrix.clone(),
                x:self.x+x,
                y:self.y+y,
                width,
                height,
                cached_hash: Default::default()
            })
        }
    }

    fn get_hash(&self) -> u64 {
        let cache = self.cached_hash.read().unwrap();
        if let Some(cache) = (*cache).as_ref() {
            return *cache;
        }

        let mut hasher = DefaultHasher::new();

        for item in self.items() {
            hasher.write_usize(item.atom_ptr());
        }

        let result = hasher.finish();

        std::mem::drop(cache);

        let mut cache = self.cached_hash.write().unwrap();
        *cache = Some(result);

        result
    }
}

impl Hash for Window {
    fn hash<H:Hasher>(&self, state: &mut H) {
        state.write_u64(self.get_hash())
    }
}

impl PartialEq for Window {
    fn eq(&self, other: &Window) -> bool {
        if self.size() != other.size() {
            false
        }
        else if self.get_hash() != other.get_hash() {
            false
        }
        else {
            self.items().eq(other.items())
        }
    }
}

impl Eq for Window {}

impl PartialOrd for Window {
    fn partial_cmp(&self, other: &Window) -> Option<Ordering> {
        let size_order = self.size().cmp(&other.size());
        if size_order != Ordering::Equal {
            return Some(size_order);
        }

        if self.eq(other) {
            return Some(Ordering::Equal);
        }

        let hash = self.get_hash();
        let other_hash = other.get_hash();

        let cmp = hash.cmp(&other_hash);

        match cmp {
            Ordering::Equal => {
                // since hashes are equal we have to resort to ordering of content.
                // Note: Atom does not (yet) implement Ord so we compare the inner atom_ptr values.
                Some(self.items().map(|a|a.atom_ptr()).cmp(other.items().map(|a|a.atom_ptr())))
            },
            _ => Some(cmp)
        }
    }
}

impl Ord for Window {
    fn cmp(&self, other: &Window) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl ArcBlobImpl for Window {
    fn write(&self, stream: &mut PrologStream) -> std::io::Result<()> {
        write!(stream, "<window {}x{} ({}+{}x{}+{})>",
               self.width,
               self.height,
               self.matrix.cols,
               self.x,
               self.matrix.rows,
               self.y)
    }

    fn compare(&self, other: &Window) -> Ordering {
        self.cmp(other)
    }
}

// maybe this should be merged with the other error?
#[derive(Debug)]
enum RangeError {
    RowOutOfRange,
    ColOutOfRange
}

impl IntoPrologException for RangeError {
    fn into_prolog_exception<'a, 'b, T: QueryableContextType>(
        self,
        context: &'a Context<'b, T>) -> PrologResult<Term<'a>> {
        match self {
            RangeError::RowOutOfRange => term!{context: error(row_out_of_range, _)},
            RangeError::ColOutOfRange => term!{context: error(col_out_of_range, _)},
        }
    }
}

trait Windowed {
    fn original(&self) -> &Matrix;
    fn offset(&self) -> (usize, usize);
    fn size(&self) -> (usize, usize);

    fn row(&self, row: usize) -> Result<&[Atom],RangeError> {
        let (x,y) = self.offset();
        let (width, height) = self.size();
        if row >= height {
            return Err(RangeError::RowOutOfRange);
        }
        let original = self.original();
        let orig_cols = original.cols;
        let offset = (y+row)*orig_cols + x;
        Ok(&self.original().data[offset..offset+width])
    }

    fn col(&self, col: usize) -> Result<Vec<&Atom>,RangeError> {
        let (x,y) = self.offset();
        let (width, height) = self.size();
        if col >= width {
            return Err(RangeError::ColOutOfRange);
        }
        let original = self.original();
        let orig_cols = original.cols;
        let mut offset = y*orig_cols + x + col;
        let mut result = Vec::with_capacity(height);

        for _ in 0..height {
            result.push(&self.original().data[offset]);
            offset += orig_cols;
        }

        Ok(result)
    }

    fn as_list_of_lists<C:QueryableContextType>(&self, context: &Context<C>, list_of_lists_term: &Term) -> PrologResult<()> {
        let frame = context.open_frame();
        let (_width, height) = self.size();
        let mut rows_vec = Vec::with_capacity(height);

        for r in 0..height {
            let row = self.row(r).expect("row out of range when it shouldn't be");

            let row_term = frame.new_term_ref();
            row_term.unify(row)?;
            rows_vec.push(row_term);
        }

        let result = list_of_lists_term.unify(rows_vec.as_slice());
        frame.close();

        result
    }

    fn items<'a>(&'a self) -> Box<dyn Iterator<Item=&Atom>+'a> {
        let (width, height) = self.size();
        let (mut x, mut y) = (0,0);

        Box::new(std::iter::from_fn(move || {
            if y == height {
                return None;
            }

            let result = &self.row(y).unwrap()[x];
            x+=1;
            if x == width {
                x = 0;
                y += 1;
            }

            Some(result)
        }))
    }
}

impl Windowed for Matrix {
    fn original(&self) -> &Matrix {
        self
    }
    fn offset(&self) -> (usize, usize) {
        (0,0)
    }

    fn size(&self) -> (usize, usize) {
        (self.cols, self.rows)
    }
}

impl Windowed for Window {
    fn original(&self) -> &Matrix {
        &self.matrix
    }
    fn offset(&self) -> (usize, usize) {
        (self.x, self.y)
    }

    fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

}

pub fn register() {
    register_as_matrix();
    register_as_list_of_lists();
    register_matrix_window();
    register_window_window();
    register_matrix_row();
    register_window_row();
    register_matrix_col();
    register_window_col();
}
