
arg <- pd_get_function_arg_ids(function.id, pd)[1]
# All siblings
pd_get_sibling_ids(arg, pd)
# move to next sibling
arg <- pd_get_next_sibling_id(arg, pd)
pd_token(arg, pd)

# and again
arg <- pd_get_next_sibling_id(arg, pd)
pd_token(arg, pd)

# too far go back
arg <- pd_get_prev_sibling_id(arg, pd)
pd_text(arg, pd)

