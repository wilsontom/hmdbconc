test_that('hmdbconc',
          {
            test_acc <- get_concentrations('0000001')

            expect_true(tibble::is_tibble(test_acc))
            expect_true(ncol(test_acc) == 9)

            expect_error(get_concentrations('00001'))

          })
