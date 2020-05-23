import requests


# Querying which actions are available on resource 0 to user with ID 0
r_available = requests.get('http://localhost:8080/available/?resource=0&user=0')

# The result is a JSON object with a key for each action branch, and
# each branch consisting of an array of strings, each element being
# an action name available to that user

print(r_available.json())
# Assuming this is the same resource as added in ** Populating Redis in
# docs/integration.org, the result would be:

# >>> print(ra.json())
# {u'data': [u'no-access', u'view']}

# To perform an action, we use the /run-action endpoint, which takes
# a branch name and action name in addition to the resource and user.

# The result of the /available query tells us exactly which branches and
# actions we can perform
r_view = requests.get('http://localhost:8080/run-action/?resource=0&user=0&branch=data&action=view')

# In this case, the output of the 'view' action is a JSON array representation of the SQL output
print(r_view.json())

# Thus, each of the fields can be accessed as in any other array; for example
# we can retrieve the Phenotype.Pre_publication_description field:
print(r_view.json()[3])


r_na = requests.get('http://localhost:8080/run-action/?resource=0&user=0&branch=data&action=no-access')

# If the user doesn't have access, the string "no-action" is returned,
# as the no-access-action is used by default
