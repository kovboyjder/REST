# ABAPGIT_API

Api for Abapgit

This API is for interactions with ABAPGIT from a CI perspective.

Create an ICF service and add this class as a handler class. 
The following parameters are mandatory
* Action - An action is the intention of the API call. The following actions are avaible
    1. BACKGROUND - Creates a background job to pull all objects from GITLAB
    2. BRANCH - Switches the branch of a given repository. Depends on a parameter **Branch** to be filled out.'
    3. PULL - Pulls a given repository, requires parameter **Username** and **Password** to be set
    4. REFRESH - Pulls the latest changes from remote, requires parameter **Username** and **Password** to be set
* Repository - The repository name. Can be the CI PARAMETER **$CI_PROJECT_NAME**


