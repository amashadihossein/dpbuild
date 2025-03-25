# properly checks valid repository 

    Code
      data_object = list()
      path <- "."
      dp_write(data_object, project_path = path)
    Condition
      Error in `dp_write()`:
      ! Not a dp repsitory. Run `dp_repository_check(".")` for details

# properly checks data object class

    Code
      data_object = list()
      path <- withr::local_tempfile()
      dp_write(data_object, project_path = path)
    Condition
      Error in `dp_write()`:
      ! dp_write requires and data_object of class dp! Use dp_structure to build a properly formatted dp

