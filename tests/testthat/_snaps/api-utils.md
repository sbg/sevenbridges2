# Utility function check_and_transform_id works

    Code
      check_and_transform_id(TRUE)
    Error <simpleError>
      Assertion on 'x' failed: Must be of type 'character' (or 'NULL'), not 'logical'.

---

    Code
      check_and_transform_id(123)
    Error <simpleError>
      Assertion on 'x' failed: Must be of type 'character' (or 'NULL'), not 'double'.

