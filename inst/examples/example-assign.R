
# The first should be an assignment
pd_is_assignment(roots[[1]], pd=pd)

# the variable/value of the assignment can be accessed by
variable.id <- pd_get_assign_variable_id(roots[[1]], pd)
value.id <- pd_get_assign_value_id(roots[[1]], pd)
# Note that these function will give the variable/value part
# for both LEFT_ASSIGN and RIGHT_ASSIGN operators, going by order
# of ids, or position in the data may not give the expected results.
