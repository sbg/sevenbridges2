# Project print method works

    Code
      setup_project_obj$print()
    Output
      Project name:  Project name 
      Project id:  project_id 

# Project detailed_print method works

    Code
      setup_project_obj$detailed_print()
    Message
      
      -- Project ---------------------------------------------------------------------
      * category: PRIVATE
      * modified_on: 2023-07-10 13:36:00
      * created_on: 2023-06-10 13:36:00
      * created_by: user1
      * root_folder: root_folder_id
      * type: v2
      * description: Project description
      * billing_group: billing group
      * name: Project name
      * id: project_id
      * settings
        * locked: FALSE
      * tags
        * tag_1: Tag1
        * tag_2: Tag2
      * permissions
        * admin: TRUE
        * execute: TRUE
        * copy: TRUE
        * read: TRUE
        * write: TRUE

