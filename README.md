Welcome to ED.
==

* Overview
  ED is a tentative project for examine concept about erlang based php process handling.
* Concept
  The concept is the erlang responsible for controlling and directing the php processes,
  and the php processes responsible for implementing the business logic. The erlang application
  scheduling the jobs and provide an internal protocol for facilitate making paralell requests
  for one task.
* Scheduled task
  You can create scheduled jobs by jobs.json file.
  Example config:
  #+BEGIN_EXAMPLE
        {
            "doc_root": "/path/of/php/files/",
            "jobs": [
                {"time": 1000,  "pfr": {"script_filename": "/index.php"}},
                {"time": 5000,  "pfr": {"script_filename": "/slowdown.php"}}
            ]
        }
  #+END_EXAMPLE
