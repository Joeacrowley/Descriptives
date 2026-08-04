# add_pairwise_sig(), derive_se(), assign_sig_letters(), exact_se_diff() and
# pairwise_test_one_group() have moved into calc_stats.R itself, right before
# the stat_registry/calc_stats() dispatch section - calc_stats() now calls
# add_pairwise_sig() automatically when pairwise = TRUE, the same way
# pval = TRUE already works. add_pairwise_sig() is still fully usable
# standalone too; it just lives in calc_stats.R now. This file is kept only
# as a pointer so nothing that used to reference it 404s outright; nothing
# here is sourced anymore.
