
# assignments have three children
# The operator, the assignment, and the value.
kids <- pd_get_children_ids(roots[[1]], pd)
# The token tells what kind of node the ids represent.
pd_token(kids, pd)
