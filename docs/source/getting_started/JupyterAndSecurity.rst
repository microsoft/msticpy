Why Use Jupyter for Security Investigations?
============================================

What is Jupyter?
----------------

Jupyter is an interactive development and data manipulation environment
hosted in a browser. It takes code that you type into a cell, executes
it and returns the output to you. Here is an example:

.. figure:: _static/JupyterAndSecurity-JupyterCell.png
   :alt: Jupyter code cell executing simple for loop

   Jupyter code cell executing simple for loop

For more introductory information and sample notebooks go to
`jupyter.org <https://jupyter.org>`__. and the `jupyter introductory
documentation <https://jupyter.readthedocs.io/en/latest/tryjupyter.html>`__

Why Jupyter?
------------

"Why would I use Jupyter notebooks to work with Azure Sentinel data
rather than the built-in query and investigation tools?" might be your
first question. And the first answer is that, usually, you wouldn't. In
most cases, the scenario and data that you are investigating can be
handled perfectly well in with the coming graphical investigation tool,
with Log Analytics queries and cool case features like Bookmarks.

One reason that you might want to reach for Jupyter is when the
complexity of what you are looking for becomes too high. "How complex is
*too complex*?" is a difficult question to answer but some guidelines
might be:

-  when the number of queries in your investigation chain goes beyond
   around 7 (the number of things that the average person can juggle in
   short-term memory).

-  when you start to need extra-strength reading glasses to see all the
   detail of the investigation graph.

-  when you discover that your browser has just crashed and you hadn't
   saved any of the queries or results that you were working on.

Some of the other benefits of working in Jupyter are outlined in the
following sections.

Data Persistence, Repeatability and Backtracking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

One of the painful things when working on a more complex security
investigation is keeping track of what you have done. You might easily
find yourself with tens of queries and results sets - many of which
turned out to be dead ends. Which ones do you keep? How easy is it to
backtrack and re-run the queries with different values or date ranges?
How do you accumulate the useful results in a single report? What if you
want to re-run the same pattern on a future investigation?

With most data-querying environments the answer is a lot of manual work
and heavy reliance on good short-term memory. Jupyter, on the other
hand, gives you a linear progression through the investigation - saving
queries and data as you go. With the use of variables through the
progression of the queries (e.g. for time ranges, account names, IP
addresses, etc.) it also makes it much easier to backtrack and re-run
and to reuse the entire workflow in future investigations.

Scripting and Programming environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In Jupyter you are not limited to querying and viewing results but
have the full power of a programming language. Although you can do
a lot in a flexible declarative language like Kql (or others like SQL),
being able to split your logic into procedural chunks is often helpful
and sometimes essential. A *declarative* language means that you need
to encode your logic in a single (possibly complex) statement, while
*procedural* languages allow you to execute logic in a series of steps.

Being able to use procedural code lets you:

- See and debug intermediate results.
- Add functionality (such as decoding fields, parsing data) that
  may not be available in the query language.
- Re-use partial results in later processing steps.

Joining to External Data
^^^^^^^^^^^^^^^^^^^^^^^^

Most of your telemetry/event data will be in Azure Sentinel workspace
tables but there will often be exceptions:

-  data in an external service that you do not own - e.g. IP whois and
   geolocation data, threat intelligence source,

-  sensitive data that may only be stored within your organization - HR
   Database, lists of execs, admins or high-value assets,

-  or simply data that you have not yet migrated to the cloud.

Any data that is accessible over your network or from a file can be
linked with Azure Sentinel data via Python and Jupyter.

Access to Sophisticated Data Processing, Machine Learning and Visualization
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Azure Sentinel and the Kusto/Log Analytics data store underlying it have
a lot of options for visualization and advanced data processing (even
clustering, windowed statistical and machine learning functions) and
more capabilities are being added all the time. However, there may be
times when you need something different: specialized visualizations,
machine learning libraries or even just data processing and
transformation facilities not available in the Azure Sentinel platform.
You can see examples of these in some of the Azure Sentinel sample
notebooks (see References at the end of the document).

Some well-known examples of these in the Python language are:

- *pandas* for data processing, cleanup and engineering
- *matplotlib*, *holoviews*, *plotly* and many others for visualization
- *numpy* and *scipy* for advanced numerical and scientific processing
- *scikit-learn* for machine learning
- *tensorflow*, *pytorch*, *keras* for deep learning

Why Python?
-----------

Jupyter can be used with many different languages - what makes Python a
good choice?

Popularity
^^^^^^^^^^

It is very likely that you already have Python coders in your
organization. It is now the most widely taught language in Computer
Science courses and used widely in many scientific fields. It is also
frequently used by IT Pros -- where it has largely replaced perl as the
go-to language for scripting and systems management -- and by web
developers (many popular services such as DropBox and Instagram are
almost entirely written in Python).

Ecosystem
^^^^^^^^^

Driven by this popularity, there is a vast repository of python
libraries available on `PyPi <https://pypi.org>`__ and nearly 1 million
python repos on `Github <https://github.com/search?q=python>`__. For
many of the tools that you need as a security investigator - data
manipulation, data analysis, visualization, machine learning and
statistical analysis - no other language ecosystem has comparable tools.

One remarkable point here is that pretty much every major python package
and the core language itself are open source and written and maintained
by volunteers.


Alternatives to Python
^^^^^^^^^^^^^^^^^^^^^^

You can use other language kernels with Juypter, and you can mix and
match languages (to a degree) within the same notebook using 'magics'
that allow execution of individual cells using another language. For
example, you could retrieve data using a PowerShell script cell, process
the data in python and use JavaScript to render a visualization. In
practice, this can be a little trickier than it sounds but certainly
possible with a bit of hand-wiring.

References
----------

- `jupyter.org <https://jupyter.org>`__
- `python <https://python.org>`__
- `PyPi <https://pypi.org>`__
- `Github <https://github.com/search?q=python>`__
- `Kusto Query Language <https://kusto.azurewebsites.net/docs/query/index.html>`__
- `pandas <https://pandas.pydata.org/>`__
- `matplotlib <https://matplotlib.org>`__
- `holoviews <https://holoviews.org>`__
- `plotly <https://plot.ly>`__
- `numpy <https://www.numpy.org>`__
- `scipy <https://www.scipy.org>`__
- `scikit-learn <https://scikit-learn.org/stable/index.html>`__
- `tensorflow <https://www.tensorflow.org/>`__
- `pytorch <https://pytorch.org>`__
- `keras <https://keras.io/>`__
