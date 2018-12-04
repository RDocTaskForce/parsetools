
function.id <- pd_get_assign_value_id(roots[[1]], pd)
pd_is_function(function.id, pd)
length(function.kids <- pd_get_children_ids(function.id, pd))
# function nodes have many because it contains
# 1. the function keyword.
# 2. the parentheses '(' and ')'
# 3. each argument name plus the equals sign and value, if given.
# 4. and finally, and expr node for the function body.
pd_token(function.kids, pd)
# even though there are only two argument since each has
# a default value given there are 6 total nodes that
# return true as function arguments, care is needed when
# dealing with function arguments.
pd_is_function_arg(function.kids, pd)
pd_get_function_arg_ids(function.id, pd)
# A simple way to identify the argument names is
pd_text(pd_get_function_arg_variable_ids(function.id, pd), pd)

# To identify the function body node.
pd_get_function_body_id(function.id, pd)

