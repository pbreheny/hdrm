if (requireNamespace("tinytest", quietly=TRUE)) {
  tinytest::test_package("hdrm", pattern="^[^_].*\\.[rR]$")
}
