use crate::change_window::verify_contracts as change_window_verify_contracts;
use swipl::prelude::*;

fn option_string_from_term(term: &Term) -> PrologResult<Option<String>> {
    let text: PrologText = term.get_ex()?;
    let s = text.to_string();
    if s == "none" {
        Ok(None)
    } else {
        Ok(Some(s))
    }
}

fn get_transaction_info<'a, C: QueryableContextType>(
    context: &'a Context<'a, C>,
    transaction_term: &Term<'a>,
) -> PrologResult<(String, Option<String>, Option<String>)> {
    let branch_key_term = context.new_term_ref();
    let current_commit_id_term = context.new_term_ref();
    let current_schema_layer_id_term = context.new_term_ref();

    context.call_once(
        pred!("api_document:verify_contracts_transaction_info/4"),
        [
            transaction_term,
            &branch_key_term,
            &current_commit_id_term,
            &current_schema_layer_id_term,
        ],
    )?;

    let branch_key: String = branch_key_term.get_ex()?;
    let current_commit_id = option_string_from_term(&current_commit_id_term)?;
    let current_schema_layer_id = option_string_from_term(&current_schema_layer_id_term)?;

    Ok((branch_key, current_commit_id, current_schema_layer_id))
}

fn collect_contract_conflict_sets<C: QueryableContextType>(
    context: &Context<C>,
    contracts_term: &Term,
) -> PrologResult<(Vec<String>, Vec<String>)> {
    let mut must_exist: Vec<String> = Vec::new();
    let mut must_not_exist: Vec<String> = Vec::new();

    // Conflict sets contain IRIs, and IRIs are represented as Prolog atoms
    // throughout the contract system. We deliberately parse them as atoms only;
    // a string IRI in a contract is a contract-generation bug and should be
    // fixed at the source, not silently coerced here.
    let must_exist_atom = atom!("must_exist");
    let must_not_exist_atom = atom!("must_not_exist");

    for contract in context.term_list_iter(contracts_term) {
        let must_exist_term = context.new_term_ref();
        if contract
            .get_dict_key_term(&must_exist_atom, &must_exist_term)
            .is_ok()
        {
            let items: Vec<Atom> = must_exist_term.get_ex()?;
            must_exist.extend(items.into_iter().map(|a| a.to_string()));
        }
        let must_not_exist_term = context.new_term_ref();
        if contract
            .get_dict_key_term(&must_not_exist_atom, &must_not_exist_term)
            .is_ok()
        {
            let items: Vec<Atom> = must_not_exist_term.get_ex()?;
            must_not_exist.extend(items.into_iter().map(|a| a.to_string()));
        }
    }

    Ok((must_exist, must_not_exist))
}

fn unify_ok_result<C: QueryableContextType>(
    _context: &Context<C>,
    result_term: &Term,
) -> PrologResult<()> {
    result_term.unify(atom!("ok"))
}

fn unify_error_result<C: QueryableContextType>(
    context: &Context<C>,
    result_term: &Term,
    error_string: &str,
) -> PrologResult<()> {
    let error_term = context.term_from_string(error_string)?;
    result_term.unify(error_term)
}

predicates! {
    #[module("$doc")]
    pub semidet fn verify_contracts(context, transaction_term, contracts_term, result_term) {
        // Empty contract list is already handled on the Prolog side, but guard anyway.
        let first_contract = context
            .term_list_iter(contracts_term)
            .next()
            .ok_or(PrologError::Failure)?;

        let pre_branch_commit_id_atom = atom!("pre_branch_commit_id");
        let pre_branch_commit_id_term = context.new_term_ref();
        let pre_branch_commit_id = if first_contract
            .get_dict_key_term(&pre_branch_commit_id_atom, &pre_branch_commit_id_term)
            .is_ok()
        {
            option_string_from_term(&pre_branch_commit_id_term)?
        } else {
            None
        };

        let pre_schema_layer_id_atom = atom!("pre_schema_layer_id");
        let pre_schema_layer_id_term = context.new_term_ref();
        let pre_schema_layer_id = if first_contract
            .get_dict_key_term(&pre_schema_layer_id_atom, &pre_schema_layer_id_term)
            .is_ok()
        {
            option_string_from_term(&pre_schema_layer_id_term)?
        } else {
            None
        };

        let (branch_key, current_commit_id, current_schema_layer_id) =
            get_transaction_info(context, transaction_term)?;

        // Schema layer check.
        if let (Some(pre), Some(current)) = (pre_schema_layer_id, current_schema_layer_id) {
            if pre != current {
                return unify_error_result(
                    context,
                    result_term,
                    "error(elaboration_stale(schema_layer_changed))",
                );
            }
        }

        // Branch commit check.
        match (pre_branch_commit_id, current_commit_id) {
            (None, _) | (_, None) => unify_ok_result(context, result_term),
            (Some(pre), Some(current)) => {
                if pre == current {
                    unify_ok_result(context, result_term)
                } else {
                    let (must_exist, must_not_exist) =
                        collect_contract_conflict_sets(context, contracts_term)?;
                    match change_window_verify_contracts(
                        &branch_key,
                        &pre,
                        &current,
                        &must_exist,
                        &must_not_exist,
                    ) {
                        Some(_) => unify_error_result(context, result_term, "error(fail_transaction)"),
                        None => unify_ok_result(context, result_term),
                    }
                }
            }
        }
    }
}

pub fn register() {
    register_verify_contracts();
}
