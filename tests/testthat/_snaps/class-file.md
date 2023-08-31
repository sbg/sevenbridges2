# File print method works

    Code
      setup_file_obj$print()
    Message <cliMessage>
      
      -- File ------------------------------------------------------------------------
      * type: file
      * parent: parent-id
      * modified_on: 2023-06-06T11:14:11Z
      * created_on: 2023-06-06T11:14:11Z
      * project: user1/project-id
      * size: 100 bytes
      * name: File name
      * id: file-id
      * href: https://api.sbgenomics.com/v2/files/file-id
      * response: NA

# File detailed_print method works

    Code
      setup_file_obj$detailed_print()
    Message <cliMessage>
      
      -- File ------------------------------------------------------------------------
      * type: file
      * parent: parent-id
      * modified_on: 2023-06-06T11:14:11Z
      * created_on: 2023-06-06T11:14:11Z
      * project: user1/project-id
      * size: 100 bytes
      * name: File name
      * id: file-id
      * href: https://api.sbgenomics.com/v2/files/file-id
      * response: NA
      * tags
        * tag_1: tag_1
      * metadata
        * sbg_public_files_category: test
        * reference_genome: HG19_Broad_variant
        * sample_id: HCC1143_1M
        * case_id: CCLE-HCC1143
        * investigation: CCLE-BRCA
      * origin
        * task: 123a1a1a-12a1-1234-a123-1234567a1a12
      * storage
        * type: PLATFORM
        * hosted_on_locations: list("aws:us-east-1")

