
# which root is a call?
pd_is_call(roots, pd)
id <- roots[pd_is_call(roots, pd)]
# not all calls are symbole calls.
pd_is_symbol_call(id, pd)
# what is the symbol being called?
pd_text(pd_get_call_symbol_id(id, pd), pd)
# what are the arguments to the call
args <- pd_get_call_arg_ids(id, pd)
pd_token(pd_get_firstborn(args, pd), pd)
pd_text(pd_get_firstborn(args, pd), pd)
