
# Get the 'setClass' call.
class.id <- pd_get_assign_value_id(roots[2], pd)
# Check to make sure that it is a function that sets a class.
pd_is_class_definition(class.id, pd)
# and that it is the setClass call.
pd_text(pd_get_call_symbol_id(class.id, pd), pd)

