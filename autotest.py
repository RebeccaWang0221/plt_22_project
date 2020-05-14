import os
tests = ["basic_test.rs", "test_cal_bool.rs", "test_cal_float.rs", "test_cal_int.rs", "test_string.rs", "test_comments.rs", "test_comparison_and_if.rs", "test_for.rs", "test_range.rs", "test_while.rs", "test_array.rs", "test_gcd.rs", "test_array2.rs", "test_reverse.rs", "test_while2.rs", "test_array3.rs", "test_list.rs", "test_array4.rs", "test_isPrime.rs"]
for testcase in tests:
    print("---------- " + testcase + " -----------")
    os.system("./rattle.native test/" + testcase)
    os.system("./a.out")
    print("---------- finish line -----------\n\n")