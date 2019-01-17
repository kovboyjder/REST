# ABAPGIT_API

Api for Abapgit

This API is for interactions with ABAPGIT from a CI perspective.

The ICF service is enabled as zabapgit_api in the root of HED and HEG.

The following parameters are mandatory
* Action - An action is the intention of the API call. The following actions are avaible
    1. PULL - Creates a background job to pull all objects from GITLAB
    2. BRANCH - Switches the branch of a given repository. Depends on a parameter **Branch** to be filled out.
* Repository - The repository name. Can be the CI PARAMETER **$CI_PROJECT_NAME**

