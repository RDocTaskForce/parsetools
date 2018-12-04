
# Find the if statement
is.if <- pd_is_if(pd$id, pd=pd)
sum(is.if)
if.id <- pd$id[is.if]

# The predicate
pd_reconstitute(pd_get_if_predicate_id(if.id, pd), pd)

# The branch for if predicate evaluates TRUE
pd_reconstitute(pd_get_if_branch_id(if.id, pd), pd)

# The alternate for if predicate evaluates FALSE
pd_reconstitute(pd_get_if_alternate_id(if.id, pd), pd)


