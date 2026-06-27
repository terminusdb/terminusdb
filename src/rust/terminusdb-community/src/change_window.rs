use std::{
    collections::{HashMap, HashSet, VecDeque},
    io,
    sync::atomic::{AtomicU64, Ordering},
    sync::Mutex,
};

use lazy_static::lazy_static;
use swipl::{atom, atom::Atom, prelude::*, result::PrologError};
use terminusdb_store_prolog::{layer::WrappedLayer, terminus_store::Layer};

lazy_static! {
    static ref CHANGE_WINDOW: Mutex<ChangeWindow> = Mutex::new(ChangeWindow::new());
}

const NONE_COMMIT_ID: &str = "none";

struct CommitChanges {
    commit_id: String,
    parent_commit_id: Option<String>,
    #[allow(dead_code)]
    schema_layer_id: Option<String>,
    #[allow(dead_code)]
    instance_layer_id: Option<String>,
    added_iris: HashSet<String>,
    removed_iris: HashSet<String>,
    active_count: usize,
}

struct BranchWindow {
    /// Oldest commit at the front, newest at the back.
    commits: VecDeque<CommitChanges>,
    /// Map from commit id to its insertion index (monotonically increasing).
    index: HashMap<String, usize>,
    /// Insertion index for the next commit added to this branch.
    next_index: usize,
    /// The insertion index of the current front of the queue.
    start_index: usize,
}

impl BranchWindow {
    fn new() -> Self {
        Self {
            commits: VecDeque::new(),
            index: HashMap::new(),
            next_index: 0,
            start_index: 0,
        }
    }

    fn get_commit(&self, commit_id: &str) -> Option<&CommitChanges> {
        let idx = self.index.get(commit_id)?;
        let pos = idx - self.start_index;
        self.commits.get(pos)
    }

    fn get_commit_mut(&mut self, commit_id: &str) -> Option<&mut CommitChanges> {
        let idx = *self.index.get(commit_id)?;
        let pos = idx - self.start_index;
        self.commits.get_mut(pos)
    }

    fn push_back(&mut self, commit: CommitChanges) {
        let idx = self.next_index;
        self.next_index += 1;
        let commit_id = commit.commit_id.clone();
        self.commits.push_back(commit);
        self.index.insert(commit_id, idx);
    }

    fn pop_front(&mut self) -> Option<CommitChanges> {
        let removed = self.commits.pop_front()?;
        self.index.remove(&removed.commit_id);
        self.start_index += 1;
        Some(removed)
    }

    fn is_empty(&self) -> bool {
        self.commits.is_empty()
    }

    fn len(&self) -> usize {
        self.commits.len()
    }
}

struct ChangeWindow {
    branches: HashMap<String, BranchWindow>,
    /// Guard id -> (branch key, commit id). `commit_id` is `None` for the
    /// synthetic "none" sentinel that represents the first commit on a branch.
    guards: HashMap<u64, (String, Option<String>)>,
    next_guard_id: AtomicU64,
}

impl ChangeWindow {
    fn new() -> Self {
        Self {
            branches: HashMap::new(),
            guards: HashMap::new(),
            next_guard_id: AtomicU64::new(1),
        }
    }

    fn lock() -> std::sync::MutexGuard<'static, ChangeWindow> {
        CHANGE_WINDOW
            .lock()
            .expect("change window mutex poisoned: a thread panicked while holding the lock")
    }

    fn branch_window(&self, branch_key: &str) -> Option<&BranchWindow> {
        self.branches.get(branch_key)
    }

    fn branch_window_mut(&mut self, branch_key: &str) -> &mut BranchWindow {
        self.branches.entry(branch_key.to_string()).or_insert_with(BranchWindow::new)
    }

    fn register_commit(
        &mut self,
        branch_key: String,
        commit_id: String,
        parent_commit_id: Option<String>,
        schema_layer_id: Option<String>,
        instance_layer_id: Option<String>,
        added_iris: HashSet<String>,
        removed_iris: HashSet<String>,
    ) {
        let branch = self.branch_window_mut(&branch_key);
        branch.push_back(CommitChanges {
            commit_id,
            parent_commit_id,
            schema_layer_id,
            instance_layer_id,
            added_iris,
            removed_iris,
            active_count: 0,
        });
        self.prune_branch(&branch_key);
    }

    fn open_commit_window(
        &mut self,
        branch_key: String,
        parent_commit_id: Option<String>,
    ) -> Option<(u64, Option<String>)> {
        let branch = self.branch_window_mut(&branch_key);

        if parent_commit_id.is_none() {
            if branch.is_empty() {
                // Insert a synthetic "none" sentinel that represents the first
                // commit. It will be removed once the first commit is complete.
                branch.push_back(CommitChanges {
                    commit_id: NONE_COMMIT_ID.to_string(),
                    parent_commit_id: None,
                    schema_layer_id: None,
                    instance_layer_id: None,
                    added_iris: HashSet::new(),
                    removed_iris: HashSet::new(),
                    active_count: 1,
                });

                let guard_id = self.next_guard_id.fetch_add(1, Ordering::SeqCst);
                self.guards.insert(guard_id, (branch_key, None));
                return Some((guard_id, None));
            }

            // Another transaction is in the process of making the first commit.
            // Wait until it has registered a real commit.
            if branch.len() == 1
                && branch.commits.back().map(|c| c.commit_id.as_str()) == Some(NONE_COMMIT_ID)
            {
                return None;
            }

            // The first commit has been registered. Open a guard for the newest
            // real commit and return that commit id as the effective parent.
            let current_commit_id = branch
                .commits
                .back()
                .expect("branch not empty after checks")
                .commit_id
                .clone();
            let commit = branch
                .get_commit_mut(&current_commit_id)
                .expect("current commit missing from branch window");
            commit.active_count += 1;

            let guard_id = self.next_guard_id.fetch_add(1, Ordering::SeqCst);
            self.guards
                .insert(guard_id, (branch_key, Some(current_commit_id.clone())));
            return Some((guard_id, Some(current_commit_id)));
        }

        let parent_id = parent_commit_id.unwrap();
        let commit = branch.get_commit_mut(&parent_id)?;
        commit.active_count += 1;

        let guard_id = self.next_guard_id.fetch_add(1, Ordering::SeqCst);
        self.guards.insert(guard_id, (branch_key, Some(parent_id.clone())));
        Some((guard_id, Some(parent_id)))
    }

    /// Open a guard for the first real commit on a branch, serializing
    /// transactions that target the initial commit. The first caller gets the
    /// initial commit and can replace it; subsequent callers wait until a real
    /// commit has been registered and then receive that commit as the effective
    /// parent.
    fn open_first_commit_window(
        &mut self,
        branch_key: String,
        initial_commit_id: String,
    ) -> Option<(u64, String)> {
        let branch = self.branch_window_mut(&branch_key);

        if branch.is_empty() {
            return None;
        }

        // Try to lock the initial commit if it is present and unguarded.
        if let Some(commit) = branch.get_commit_mut(&initial_commit_id) {
            if commit.active_count == 0 {
                commit.active_count += 1;
                let guard_id = self.next_guard_id.fetch_add(1, Ordering::SeqCst);
                self.guards
                    .insert(guard_id, (branch_key.clone(), Some(initial_commit_id.clone())));
                return Some((guard_id, initial_commit_id));
            }
        }

        // The initial commit is already locked. Wait until the first real
        // commit has been registered.
        if branch.len() == 1
            && branch
                .commits
                .back()
                .map(|c| c.commit_id.as_str())
                == Some(&initial_commit_id)
        {
            return None;
        }

        // A real commit has been registered. Open a guard for the newest one.
        let current_commit_id = branch
            .commits
            .back()
            .expect("branch not empty after checks")
            .commit_id
            .clone();
        let commit = branch
            .get_commit_mut(&current_commit_id)
            .expect("current commit missing from branch window");
        commit.active_count += 1;

        let guard_id = self.next_guard_id.fetch_add(1, Ordering::SeqCst);
        self.guards
            .insert(guard_id, (branch_key, Some(current_commit_id.clone())));
        Some((guard_id, current_commit_id))
    }

    fn close_commit_window(&mut self, guard_id: u64) {
        let (branch_key, commit_id) = self
            .guards
            .remove(&guard_id)
            .expect("close_commit_window: guard id not found (double close or leaked guard)");

        let branch = self
            .branches
            .get_mut(&branch_key)
            .expect("close_commit_window: branch window missing");

        if let Some(commit_id) = commit_id {
            let commit = branch
                .get_commit_mut(&commit_id)
                .expect("close_commit_window: commit missing from branch window");
            assert!(
                commit.active_count > 0,
                "close_commit_window: active count for commit {} was already zero; \
                 this is a bug in the change window lifecycle, please report it",
                commit_id
            );
            commit.active_count -= 1;
        } else {
            let sentinel = branch
                .get_commit_mut(NONE_COMMIT_ID)
                .expect("close_commit_window: none sentinel missing from branch window");
            assert!(
                sentinel.active_count > 0,
                "close_commit_window: none sentinel active count was already zero; \
                 this is a bug in the change window lifecycle, please report it"
            );
            sentinel.active_count -= 1;
        }

        self.prune_branch(&branch_key);
    }

    fn intersects(
        &self,
        branch_key: &str,
        pre_commit_id: &str,
        current_commit_id: &str,
        must_exist: &HashSet<String>,
        must_not_exist: &HashSet<String>,
    ) -> Option<String> {
        let branch = self.branch_window(branch_key)?;

        let mut current = current_commit_id.to_string();
        loop {
            if current == pre_commit_id {
                return None;
            }

            let commit = branch.get_commit(&current)?;

            if commit
                .added_iris
                .intersection(must_not_exist)
                .next()
                .is_some()
            {
                return Some(current);
            }
            if commit
                .removed_iris
                .intersection(must_exist)
                .next()
                .is_some()
            {
                return Some(current);
            }

            match &commit.parent_commit_id {
                Some(parent) => current = parent.clone(),
                None => {
                    // Reached the root. If pre_commit_id is the synthetic
                    // "none" sentinel, the current commit is a descendant of
                    // the empty branch, so there is no intersection.
                    if pre_commit_id == NONE_COMMIT_ID {
                        return None;
                    }
                    // Otherwise, pre_commit_id is not an ancestor of the
                    // current commit; the window has been pruned or the
                    // branch was reset.
                    return Some(current_commit_id.to_string());
                }
            }
        }
    }

    fn prune_branch(&mut self, branch_key: &str) {
        let branch = match self.branches.get_mut(branch_key) {
            Some(branch) => branch,
            None => return,
        };

        // Determine which commits must be retained. A commit is kept if it is
        // active, or if it is the newest commit on the branch, or if it is an
        // ancestor of any kept commit. This ensures that intersects can walk
        // the parent chain from the current head back to any ancestor that a
        // transaction may have started from.
        let mut keep = HashSet::new();
        for commit in branch.commits.iter() {
            if commit.active_count > 0 {
                keep.insert(commit.commit_id.clone());
            }
        }
        if let Some(back) = branch.commits.back() {
            keep.insert(back.commit_id.clone());
        }
        let mut to_walk: Vec<String> = keep.iter().cloned().collect();
        while let Some(commit_id) = to_walk.pop() {
            if let Some(commit) = branch.get_commit(&commit_id) {
                if let Some(parent) = &commit.parent_commit_id {
                    if keep.insert(parent.clone()) {
                        to_walk.push(parent.clone());
                    }
                }
            }
        }

        // Remove contiguous oldest commits that are not in the keep set.
        while branch.len() > 1 {
            let front = branch.commits.front().unwrap();
            if keep.contains(&front.commit_id) {
                break;
            }
            branch.pop_front();
        }

        // If the only remaining commit is the synthetic "none" sentinel with
        // no active work, clean it up as well.
        if branch.len() == 1 {
            let front = branch.commits.front().unwrap();
            if front.active_count == 0 && front.commit_id == NONE_COMMIT_ID {
                branch.pop_front();
            }
        }
    }

    fn assert_empty(&self) {
        assert!(
            self.guards.is_empty(),
            "change window shutdown: {} guard(s) still open; \
             this is a bug in the change window lifecycle, please report it",
            self.guards.len()
        );

        for (branch_key, branch) in &self.branches {
            let active_count: usize = branch.commits.iter().map(|c| c.active_count).sum();
            assert!(
                active_count == 0,
                "change window shutdown: branch {} still has {} active guard(s); \
                 this is a bug in the change window lifecycle, please report it",
                branch_key,
                active_count
            );
        }
    }
}

fn layer_subject_changes(layer: &WrappedLayer) -> io::Result<(HashSet<String>, HashSet<String>)> {
    // Match layer merge semantics: process removals first, then additions.
    let mut removed = HashSet::new();
    for triple in layer.triple_removals()? {
        if let Some(subject) = layer.id_subject(triple.subject) {
            removed.insert(subject);
        }
    }

    let mut added = HashSet::new();
    for triple in layer.triple_additions()? {
        if let Some(subject) = layer.id_subject(triple.subject) {
            added.insert(subject);
        }
    }

    Ok((added, removed))
}

fn option_string_from_atom(term: &Term) -> PrologResult<Option<String>> {
    let atom: Atom = term.get_ex()?;
    if atom == atom!("none") {
        Ok(None)
    } else {
        Ok(Some(atom.to_string()))
    }
}

predicates! {
    #[module("$change_window")]
    pub semidet fn register_commit(
        _context,
        branch_key_term,
        commit_id_term,
        parent_commit_id_term,
        schema_layer_id_term,
        instance_layer_id_term,
        added_iris_term,
        removed_iris_term,
    ) {
        let branch_key: String = branch_key_term.get_ex()?;
        let commit_id: String = commit_id_term.get_ex()?;
        let parent_commit_id: Option<String> = option_string_from_atom(parent_commit_id_term)?;
        let schema_layer_id: Option<String> = option_string_from_atom(schema_layer_id_term)?;
        let instance_layer_id: Option<String> = option_string_from_atom(instance_layer_id_term)?;
        let added_iris: Vec<String> = added_iris_term.get_ex()?;
        let removed_iris: Vec<String> = removed_iris_term.get_ex()?;

        let mut window = ChangeWindow::lock();
        window.register_commit(
            branch_key,
            commit_id,
            parent_commit_id,
            schema_layer_id,
            instance_layer_id,
            added_iris.into_iter().collect(),
            removed_iris.into_iter().collect(),
        );
        Ok(())
    }

    #[module("$change_window")]
    pub semidet fn layer_changes(
        context,
        layer_term,
        added_iris_term,
        removed_iris_term,
    ) {
        let layer: WrappedLayer = layer_term.get_ex()?;
        let (added, removed) = context.try_or_die(layer_subject_changes(&layer))?;
        let added: Vec<String> = added.into_iter().collect();
        let removed: Vec<String> = removed.into_iter().collect();
        added_iris_term.unify(added.as_slice())?;
        removed_iris_term.unify(removed.as_slice())
    }

    #[module("$change_window")]
    pub semidet fn open_commit_window(
        _context,
        branch_key_term,
        parent_commit_id_term,
        guard_id_term,
        current_commit_id_term,
    ) {
        let branch_key: String = branch_key_term.get_ex()?;
        let parent_commit_id: Option<String> = option_string_from_atom(parent_commit_id_term)?;

        let mut window = ChangeWindow::lock();
        match window.open_commit_window(branch_key, parent_commit_id) {
            Some((guard_id, current_commit_id)) => {
                guard_id_term.unify(guard_id)?;
                match current_commit_id {
                    Some(commit_id) => current_commit_id_term.unify(Atom::new(&commit_id)),
                    None => current_commit_id_term.unify(atom!(NONE_COMMIT_ID)),
                }
            }
            None => Err(PrologError::Failure),
        }
    }

    #[module("$change_window")]
    pub semidet fn open_first_commit_window(
        _context,
        branch_key_term,
        initial_commit_id_term,
        guard_id_term,
        current_commit_id_term,
    ) {
        let branch_key: String = branch_key_term.get_ex()?;
        let initial_commit_id: String = initial_commit_id_term.get_ex()?;

        let mut window = ChangeWindow::lock();
        match window.open_first_commit_window(branch_key, initial_commit_id) {
            Some((guard_id, current_commit_id)) => {
                guard_id_term.unify(guard_id)?;
                current_commit_id_term.unify(Atom::new(&current_commit_id))
            }
            None => Err(PrologError::Failure),
        }
    }

    #[module("$change_window")]
    pub semidet fn close_commit_window(_context, guard_id_term) {
        let guard_id: u64 = guard_id_term.get_ex()?;
        let mut window = ChangeWindow::lock();
        window.close_commit_window(guard_id);
        Ok(())
    }

    #[module("$change_window")]
    pub semidet fn intersects(
        _context,
        branch_key_term,
        pre_commit_id_term,
        current_commit_id_term,
        must_exist_term,
        must_not_exist_term,
        intersecting_commit_id_term,
    ) {
        let branch_key: String = branch_key_term.get_ex()?;
        let pre_commit_id: String = pre_commit_id_term.get_ex()?;
        let current_commit_id: String = current_commit_id_term.get_ex()?;
        let must_exist: Vec<String> = must_exist_term.get_ex()?;
        let must_not_exist: Vec<String> = must_not_exist_term.get_ex()?;

        let must_exist: HashSet<String> = must_exist.into_iter().collect();
        let must_not_exist: HashSet<String> = must_not_exist.into_iter().collect();

        let window = ChangeWindow::lock();
        match window.intersects(
            &branch_key,
            &pre_commit_id,
            &current_commit_id,
            &must_exist,
            &must_not_exist,
        ) {
            Some(commit_id) => intersecting_commit_id_term.unify(commit_id),
            None => Err(PrologError::Failure),
        }
    }

    #[module("$change_window")]
    pub semidet fn change_window_assert_empty(_context) {
        let window = ChangeWindow::lock();
        window.assert_empty();
        Ok(())
    }
}

pub fn register() {
    register_register_commit();
    register_layer_changes();
    register_open_commit_window();
    register_open_first_commit_window();
    register_close_commit_window();
    register_intersects();
    register_change_window_assert_empty();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intersects_walks_parent_chain() {
        let mut window = ChangeWindow::new();
        let branch = window.branch_window_mut("branch");

        let mut added = HashSet::new();
        added.insert("http://example.com/data/world/Person/Duke".to_string());
        branch.push_back(CommitChanges {
            commit_id: "a".to_string(),
            parent_commit_id: None,
            schema_layer_id: None,
            instance_layer_id: None,
            added_iris: added,
            removed_iris: HashSet::new(),
            active_count: 0,
        });

        branch.push_back(CommitChanges {
            commit_id: "b".to_string(),
            parent_commit_id: Some("a".to_string()),
            schema_layer_id: None,
            instance_layer_id: None,
            added_iris: HashSet::new(),
            removed_iris: HashSet::new(),
            active_count: 0,
        });

        let mut must_not_exist = HashSet::new();
        must_not_exist.insert("http://example.com/data/world/Person/Duke".to_string());

        let result = window.intersects("branch", "none", "b", &HashSet::new(), &must_not_exist);
        assert_eq!(result, Some("a".to_string()));
    }
}
