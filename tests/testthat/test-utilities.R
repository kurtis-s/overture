context("test-utilities")

test_that("AcceptProposal works", {
    # Always accept when the Metropolis ratio is >= 1
    expect_true(AcceptProposal(log(1), log(10)))
    expect_true(AcceptProposal(log(2), log(1), log(1), log(2)))
    expect_true(AcceptProposal(log(2), log(1), log(1), log(10)))

    # Tests for Metropolis ratio < 1
    m1 <- mockery::mock(0.4, 0.6, 0.2, 0.3)
    mockery::stub(AcceptProposal, "stats::runif", m1)
    expect_true(AcceptProposal(log(2), log(1)))
    expect_false(AcceptProposal(log(2), log(1)))
    expect_true(AcceptProposal(log(2), log(1), log(2), log(1)))
    expect_false(AcceptProposal(log(2), log(1), log(2), log(1)))
})
