data PyType =
        Scalar {
            type_name :: Maybe String,
            type_structure :: Map String PyType
        }
      | Functional {
          type_arguments :: [PyType],
          type_return :: PyType
        }
      | Alpha {
          type_name :: String,
          type_wrapped :: PyType
        }
      | Any
      | Void
