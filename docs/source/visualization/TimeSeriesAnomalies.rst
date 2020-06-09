Time Series Analysis and Anomalies Visualization
================================================

This notebook demonstrates the time series analysis and anomalies
visualization built using the `Bokeh
library <https://bokeh.pydata.org>`__ as well as using built-in native
KQL operators.

Time Series analysis generally involves below steps
 - Generating TimeSeries Data
 - Use Time Series Analysis functions to discover anomalies
 - Visualize Time Series anomalies

Read more about time series analysis in detail from reference microsoft
TechCommunity blog posts

**Reference Blog Posts:**

- `Looking for unknown anomalies - what is normal? Time Series analysis & its applications in Security <https://techcommunity.microsoft.com/t5/azure-sentinel/looking-for-unknown-anomalies-what-is-normal-time-series/ba-p/555052>`__

- `Time Series visualization of Palo Alto logs to detect data exfiltration <https://techcommunity.microsoft.com/t5/azure-sentinel/time-series-visualization-of-palo-alto-logs-to-detect-data/ba-p/666344>`__

.. code:: ipython3

    # Imports
    import sys
    import warnings

    from msticpy.common.utility import check_py_version

    MIN_REQ_PYTHON = (3, 6)
    check_py_version(MIN_REQ_PYTHON)

    from IPython import get_ipython
    from IPython.display import display, HTML, Markdown
    import ipywidgets as widgets

    import pandas as pd

    #setting pandas display options for dataframe
    pd.set_option("display.max_rows", 100)
    pd.set_option("display.max_columns", 50)
    pd.set_option("display.max_colwidth", 100)

    # msticpy imports
    from msticpy.data import QueryProvider
    from msticpy.nbtools import *
    from msticpy.sectools import *
    from msticpy.nbtools.wsconfig import WorkspaceConfig
    from msticpy.nbtools.timeseries import display_timeseries_anomolies

    WIDGET_DEFAULTS = {
        "layout": widgets.Layout(width="95%"),
        "style": {"description_width": "initial"},
    }

    #Adjusting width of the screen
    display(HTML("<style>.container { width:80% !important; }</style>"))

    ws_config = WorkspaceConfig()


.. code:: ipython3

    # Authentication
    qry_prov = QueryProvider(data_environment="LogAnalytics")
    qry_prov.connect(connection_str=ws_config.code_connect_str)

Generating Time Series Data
---------------------------

Time Series is a series of data points indexed (or listed or graphed) in
time order. The data points are often discrete numeric points such as
frequency of counts or occurrences against a timestamp column of the
dataset

Using LogAnalytics Query Provider
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

msticpy has a QueryProvider through which you can connect to LogAnalytics
Data environment. via ``QueryProvider(data_environment="LogAnalytics")``
Once you connect to data environment (``qry_prov.connect()``), you can
list the available queries (``qry_prov.list_queries()``) for the data
environment which in this case is LogAnalytics.

Displaying available timeseries queries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For this notebook, we are interested in time series queries only, so we
will filter and display only those.

.. code:: ipython3

    queries = qry_prov.list_queries()
    for query in queries:
        if "timeseries" in query:
            print(query)


.. parsed-literal::

    MultiDataSource.get_timeseries_anomalies
    MultiDataSource.get_timeseries_data
    MultiDataSource.get_timeseries_decompose
    MultiDataSource.plot_timeseries_datawithbaseline
    MultiDataSource.plot_timeseries_scoreanomolies


Get TimeSeries Data from LogAnalytics Table
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can get more details about the individual query by executing
``qry_prov.MultiDataSource.get_timeseries_data('?')`` which will display
Query, data source, parameters and parameterized raw KQL query

::

    Query:  get_timeseries_data
    Data source:  LogAnalytics
    Retrieves TimeSeriesData prepared to use with built-in KQL time series functions

    Parameters
    ----------
    add_query_items: str (optional)
        Additional query clauses
    aggregatecolumn: str (optional)
        field to agregate from source dataset
        (default value is: Total)
    aggregatefunction: str (optional)
        Aggregation functions to use - count(), sum(), avg() etc
        (default value is: count())
    end: datetime
        Query end time
    groupbycolumn: str (optional)
        Group by field to aggregate results
        (default value is: Type)
    scorethreshold: str (optional)
        Score threshold for alerting
        (default value is: 3)
    start: datetime
        Query start time
    table: str
        Table name
    timeframe: str (optional)
        Aggregation TimeFrame
        (default value is: 1h)
    timestampcolumn: str (optional)
        Timestamp field to use from source dataset
        (default value is: TimeGenerated)
    where_clause: str (optional)
        Optional additional filter clauses
    Query:
    {table} {where_clause} | project {timestampcolumn},{aggregatecolumn},{groupbycolumn} | where {timestampcolumn} >= datetime({start}) | where {timestampcolumn} <= datetime({end}) | make-series {aggregatecolumn}={aggregatefunction} on {timestampcolumn} from datetime({start}) to datetime({end}) step {timeframe} by {groupbycolumn} {add_query_items}

.. code:: ipython3

    #Specify start and end timestamps
    start='2020-02-09 00:00:00.000000'
    end='2020-03-10 00:00:00.000000'
    #Execute the query by passing required and optional parameters
    time_series_data = qry_prov.MultiDataSource.get_timeseries_data(
    start=start,
    end=end,
    table="CommonSecurityLog",
    timestampcolumn="TimeGenerated",
    aggregatecolumn="SentBytes",
    groupbycolumn="DeviceVendor",
    aggregatefunction="sum(SentBytes)",
    where_clause='|where DeviceVendor=="Palo Alto Networks"',
    add_query_items='|mv-expand TimeGenerated to typeof(datetime), SentBytes to typeof(long)',
    )
    #display the output
    time_series_data


.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>DeviceVendor</th>
          <th>SentBytes</th>
          <th>TimeGenerated</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>Palo Alto Networks</td>
          <td>[2169225531, 2157438780, 2190010184, 2312862664, 2173326723, 2205690775, 2134192633, 2289092642,...</td>
          <td>[2020-02-09T00:00:00.0000000Z, 2020-02-09T01:00:00.0000000Z, 2020-02-09T02:00:00.0000000Z, 2020-...</td>
        </tr>
      </tbody>
    </table>
    </div>


Time Series Analysis and discovering Anomalies
----------------------------------------------

By analyzing time series data over an extended period, we can identify
time-based patterns (e.g. seasonality, trend etc.) in the data and
extract meaningful statistics which can help in flagging outliers. A
particular example in a security context is user logon patterns over a
period of time exhibiting different behavior after hours and on
weekends: computing deviations from these changing patterns is rather
difficult in traditional atomic detections with static thresholds. KQL
built-in functions can automatically identify such seasonality and trend
from the input data and take it into consideration when flagging
anomalies.

Using Built-in KQL to generate TimeSeries decomposition
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this case, we will use built-in KQL function ``series_decompose()``
to decompose time series to generate additional data points such as
baseline, seasonal , trend etc.

**KQL Reference Documentation:** -
`series_decompose <https://docs.microsoft.com/en-us/azure/kusto/query/series-decomposefunction>`__

You can use available query
``qry_prov.MultiDataSource.plot_timeseries_datawithbaseline()`` to get
the similar details

::

   Query:  plot_timeseries_datawithbaseline
   Data source:  LogAnalytics
   Plot timeseries data using built-in KQL time series decomposition using built-in KQL render method

   Parameters
   ----------
   aggregatecolumn: str (optional)
       field to agregate from source dataset
       (default value is: Total)
   aggregatefunction: str (optional)
       Aggregation functions to use - count(), sum(), avg() etc
       (default value is: count())
   end: datetime
       Query end time
   groupbycolumn: str (optional)
       Group by field to aggregate results
       (default value is: Type)
   scorethreshold: str (optional)
       Score threshold for alerting
       (default value is: 3)
   start: datetime
       Query start time
   table: str
       Table name
   timeframe: str (optional)
       Aggregation TimeFrame
       (default value is: 1h)
   timestampcolumn: str (optional)
       Timestamp field to use from source dataset
       (default value is: TimeGenerated)
   where_clause: str (optional)
       Optional additional filter clauses
   Query:
    {table} {where_clause} | project {timestampcolumn},{aggregatecolumn},{groupbycolumn} | where {timestampcolumn} >= datetime({start}) | where {timestampcolumn} <= datetime({end}) | make-series {aggregatecolumn}={aggregatefunction} on {timestampcolumn} from datetime({start}) to datetime({end}) step {timeframe} by {groupbycolumn} | extend (baseline,seasonal,trend,residual) = series_decompose({aggregatecolumn}) | mv-expand {aggregatecolumn} to typeof(double), {timestampcolumn} to typeof(datetime), baseline to typeof(long), seasonal to typeof(long), trend to typeof(long), residual to typeof(long) | project {timestampcolumn}, {aggregatecolumn}, baseline | render timechart with (title="Time Series Decomposition - Baseline vs Observed TimeChart")

.. code:: ipython3

    time_series_baseline= qry_prov.MultiDataSource.plot_timeseries_datawithbaseline(start=start, end =end, table='CommonSecurityLog',timestampcolumn = 'TimeGenerated', aggregatecolumn='SentBytes',groupbycolumn='DeviceVendor',aggregatefunction='sum(SentBytes)', scorethreshold='1.5', where_clause='|where DeviceVendor=="Palo Alto Networks"')
    time_series_baseline.head()




.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>TimeGenerated</th>
          <th>SentBytes</th>
          <th>baseline</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>2020-02-09 00:00:00</td>
          <td>2.169226e+09</td>
          <td>2205982717</td>
        </tr>
        <tr>
          <th>1</th>
          <td>2020-02-09 01:00:00</td>
          <td>2.157439e+09</td>
          <td>2205982717</td>
        </tr>
        <tr>
          <th>2</th>
          <td>2020-02-09 02:00:00</td>
          <td>2.190010e+09</td>
          <td>2205982717</td>
        </tr>
        <tr>
          <th>3</th>
          <td>2020-02-09 03:00:00</td>
          <td>2.312863e+09</td>
          <td>2205982717</td>
        </tr>
        <tr>
          <th>4</th>
          <td>2020-02-09 04:00:00</td>
          <td>2.173327e+09</td>
          <td>2205982717</td>
        </tr>
      </tbody>
    </table>
    </div>

Using MSTICPY - Seasonal-Trend decomposition using LOESS (STL)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this case, we will use msticpy function `timeseries_anomalies_stl` which leverages `STL` method from `statsmodels` API to decompose a time series into three components: trend, seasonal and residual. STL uses LOESS (locally estimated scatterplot smoothing) to extract smooths estimates of the three components. The key inputs into STL are:

- season - The length of the seasonal smoother. Must be odd.
- trend - The length of the trend smoother, usually around 150% of season. Must be odd and larger than season.
- low_pass - The length of the low-pass estimation window, usually the smallest odd number larger than the periodicity of the data.

More info : https://www.statsmodels.org/dev/generated/statsmodels.tsa.seasonal.STL.html#statsmodels.tsa.seasonal.STL

Documentation of timeseries_anomalies_stl function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

timeseries_anomalies_stl(data: pandas.core.frame.DataFrame, **kwargs) -> pandas.core.frame.DataFrame
    Discover anomalies in Timeseries data using
    STL (Seasonal-Trend Decomposition using LOESS) method using statsmodels package.
    
    Parameters
    ----------
    data: pd.DataFrame
        DataFrame as a time series data set retrived from data connector or external data source.
        Dataframe must have 2 columns with time column set as index and other numeric value.
    
    Other Parameters
    ----------------
    seasonal: int, optional
        Seasonality period of the input data required for STL. 
        Must be an odd integer, and should normally be >= 7 (default).
    period: int, optional
        Periodicity of the the input data. by default 24 (Hourly).
    score_threshold: float, optional
        standard deviation threshold value calculated using Z-score used to flag anomalies,
        by default 3
    
    Returns
    -------
    pd.DataFrame
        Returns a dataframe with additional columns by decomposing time series data
        into residual, trend, seasonal, weights, baseline, score and anomalies.
        The anomalies column will have 0,1,-1 values based on score_threshold set.

.. code:: ipython3

    # Read Time series data with date as index and other column
    stldemo = pd.read_csv(
        "data/TimeSeriesDemo.csv", index_col=["TimeGenerated"], usecols=["TimeGenerated","TotalBytesSent"])
    stldemo.head()

.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }
    
        .dataframe tbody tr th {
            vertical-align: top;
        }
    
        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>TotalBytesSent</th>
        </tr>
        <tr>
          <th>TimeGenerated</th>
          <th></th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>2019-05-01T06:00:00Z</th>
          <td>873713587</td>
        </tr>
        <tr>
          <th>2019-05-01T07:00:00Z</th>
          <td>882187669</td>
        </tr>
        <tr>
          <th>2019-05-01T08:00:00Z</th>
          <td>852506841</td>
        </tr>
        <tr>
          <th>2019-05-01T09:00:00Z</th>
          <td>898793650</td>
        </tr>
        <tr>
          <th>2019-05-01T10:00:00Z</th>
          <td>891598085</td>
        </tr>
      </tbody>
    </table>
    </div>



Discover anomalies using timeseries_anomalies_stl function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We will run msticpy function `timeseries_anomalies_stl` on the input data to discover anomalies.

.. code:: ipython3

    output = timeseries_anomalies_stl(stldemo)
    output.head()

.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }
    
        .dataframe tbody tr th {
            vertical-align: top;
        }
    
        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>TimeGenerated</th>
          <th>TotalBytesSent</th>
          <th>residual</th>
          <th>trend</th>
          <th>seasonal</th>
          <th>weights</th>
          <th>baseline</th>
          <th>score</th>
          <th>anomalies</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>2019-05-01T06:00:00Z</td>
          <td>873713587</td>
          <td>-7258970</td>
          <td>786685528</td>
          <td>94287029</td>
          <td>1</td>
          <td>880972557</td>
          <td>-0.097114</td>
          <td>0</td>
        </tr>
        <tr>
          <th>1</th>
          <td>2019-05-01T07:00:00Z</td>
          <td>882187669</td>
          <td>2291183</td>
          <td>789268398</td>
          <td>90628087</td>
          <td>1</td>
          <td>879896485</td>
          <td>0.029661</td>
          <td>0</td>
        </tr>
        <tr>
          <th>2</th>
          <td>2019-05-01T08:00:00Z</td>
          <td>852506841</td>
          <td>-2875384</td>
          <td>791851068</td>
          <td>63531157</td>
          <td>1</td>
          <td>855382225</td>
          <td>-0.038923</td>
          <td>0</td>
        </tr>
        <tr>
          <th>3</th>
          <td>2019-05-01T09:00:00Z</td>
          <td>898793650</td>
          <td>17934415</td>
          <td>794432848</td>
          <td>86426386</td>
          <td>1</td>
          <td>880859234</td>
          <td>0.237320</td>
          <td>0</td>
        </tr>
        <tr>
          <th>4</th>
          <td>2019-05-01T10:00:00Z</td>
          <td>891598085</td>
          <td>8677706</td>
          <td>797012590</td>
          <td>85907788</td>
          <td>1</td>
          <td>882920378</td>
          <td>0.114440</td>
          <td>0</td>
        </tr>
      </tbody>
    </table>
    </div>



Displaying Anomalies using STL
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We will filter only the anomalies (with value 1 from anomalies column) of the output dataframe retrieved after running the msticpy function `timeseries_anomalies_stl`

.. code:: ipython3

    output[output['anomalies']==1]

.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }
    
        .dataframe tbody tr th {
            vertical-align: top;
        }
    
        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>TimeGenerated</th>
          <th>TotalBytesSent</th>
          <th>residual</th>
          <th>trend</th>
          <th>seasonal</th>
          <th>weights</th>
          <th>baseline</th>
          <th>score</th>
          <th>anomalies</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>299</th>
          <td>2019-05-13T17:00:00Z</td>
          <td>916767394</td>
          <td>288355070</td>
          <td>523626111</td>
          <td>104786212</td>
          <td>1</td>
          <td>628412323</td>
          <td>3.827062</td>
          <td>1</td>
        </tr>
        <tr>
          <th>399</th>
          <td>2019-05-17T21:00:00Z</td>
          <td>1555286702</td>
          <td>296390627</td>
          <td>1132354860</td>
          <td>126541214</td>
          <td>1</td>
          <td>1258896074</td>
          <td>3.933731</td>
          <td>1</td>
        </tr>
        <tr>
          <th>599</th>
          <td>2019-05-26T05:00:00Z</td>
          <td>1768911488</td>
          <td>347810809</td>
          <td>1300005332</td>
          <td>121095345</td>
          <td>1</td>
          <td>1421100678</td>
          <td>4.616317</td>
          <td>1</td>
        </tr>
      </tbody>
    </table>
    </div>


Read From External Sources
^^^^^^^^^^^^^^^^^^^^^^^^^^

If you have time series data in other locations, you can read it via
pandas or respective data store API where data is stored. The pandas I/O
API is a set of top level reader functions accessed like
pandas.read_csv() that generally return a pandas object.

Read More at Pandas Documentation: - `I/O Tools (Text
,CSV,HDF5..) <https://pandas.pydata.org/pandas-docs/stable/user_guide/io.html>`__

Example of using Pandas ``read_csv`` to read local csv file containing
TimeSeries demo dataset. Additional columns in the csv such as
``baseline``, ``score`` and ``anoamlies`` are generated using built-in
KQL Time series functions such as ``series_decompose_anomalies()``.

.. code:: ipython3

    timeseriesdemo = pd.read_csv('TimeSeriesDemo.csv',
                              parse_dates=["TimeGenerated"],
                              infer_datetime_format=True)
    timeseriesdemo.head()




.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>TimeGenerated</th>
          <th>TotalBytesSent</th>
          <th>baseline</th>
          <th>score</th>
          <th>anomalies</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>2019-05-01 06:00:00</td>
          <td>873713587</td>
          <td>782728212</td>
          <td>0.224776</td>
          <td>0</td>
        </tr>
        <tr>
          <th>1</th>
          <td>2019-05-01 07:00:00</td>
          <td>882187669</td>
          <td>838492449</td>
          <td>0.000000</td>
          <td>0</td>
        </tr>
        <tr>
          <th>2</th>
          <td>2019-05-01 08:00:00</td>
          <td>852506841</td>
          <td>816772273</td>
          <td>0.000000</td>
          <td>0</td>
        </tr>
        <tr>
          <th>3</th>
          <td>2019-05-01 09:00:00</td>
          <td>898793650</td>
          <td>878871426</td>
          <td>0.000000</td>
          <td>0</td>
        </tr>
        <tr>
          <th>4</th>
          <td>2019-05-01 10:00:00</td>
          <td>891598085</td>
          <td>862639955</td>
          <td>0.000000</td>
          <td>0</td>
        </tr>
      </tbody>
    </table>
    </div>


Displaying Time Series anomaly alerts
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

You can also use ``series_decompose_anomalies()`` which will run Anomaly
Detection based on series decomposition. This takes an expression
containing a series (dynamic numerical array) as input and extract
anomalous points with scores.

**KQL Reference Documentation:** -
`series_decompose_anomalies <https://docs.microsoft.com/en-us/azure/kusto/query/series-decompose-anomaliesfunction>`__

You can use available query
``qry_prov.MultiDataSource.get_timeseries_alerts()`` to get the similar
details

::

   Query:  get_timeseries_alerts
   Data source:  LogAnalytics
   Time Series anomaly alerts generated using built-in KQL time series functions

   Parameters
   ----------
   aggregatecolumn: str (optional)
       field to agregate from source dataset
       (default value is: Total)
   aggregatefunction: str (optional)
       Aggregation functions to use - count(), sum(), avg() etc
       (default value is: count())
   end: datetime
       Query end time
   groupbycolumn: str (optional)
       Group by field to aggregate results
       (default value is: Type)
   scorethreshold: str (optional)
       Score threshold for alerting
       (default value is: 3)
   start: datetime
       Query start time
   table: str
       Table name
   timeframe: str (optional)
       Aggregation TimeFrame
       (default value is: 1h)
   timestampcolumn: str (optional)
       Timestamp field to use from source dataset
       (default value is: TimeGenerated)
   where_clause: str (optional)
       Optional additional filter clauses
   Query:
    {table} {where_clause} | project {timestampcolumn},{aggregatecolumn},{groupbycolumn} | where {timestampcolumn} >= datetime({start}) | where {timestampcolumn} <= datetime({end}) | make-series {aggregatecolumn}={aggregatefunction} on {timestampcolumn} from datetime({start}) to datetime({end}) step {timeframe} by {groupbycolumn} | extend (anomalies, score, baseline) = series_decompose_anomalies({aggregatecolumn}, {scorethreshold},-1,"linefit") | mv-expand {aggregatecolumn} to typeof(double), {timestampcolumn} to typeof(datetime), anomalies to typeof(double), score to typeof(double), baseline to typeof(long) | where anomalies > 0 | extend score = round(score,2)

.. code:: ipython3

    time_series_alerts= qry_prov.MultiDataSource.get_timeseries_alerts(start=start, end =end, table='CommonSecurityLog',timestampcolumn = 'TimeGenerated', aggregatecolumn='SentBytes',groupbycolumn='DeviceVendor',aggregatefunction='sum(SentBytes)', scorethreshold='1.5', where_clause='|where DeviceVendor=="Palo Alto Networks"')
    time_series_alerts






.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>DeviceVendor</th>
          <th>SentBytes</th>
          <th>TimeGenerated</th>
          <th>anomalies</th>
          <th>score</th>
          <th>baseline</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>0</th>
          <td>Palo Alto Networks</td>
          <td>2.318680e+09</td>
          <td>2020-03-09 23:00:00</td>
          <td>1.0</td>
          <td>1.52</td>
          <td>2204764145</td>
        </tr>
      </tbody>
    </table>
    </div>


Displaying Anomalies Separately
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We will filter only the anomalies shown in the above plot and display
below along with associated aggreageted hourly timewindow. You can later
query for the time windows scope for additional alerts triggered or any
other suspicious activity from other datasources.

.. code:: ipython3

    timeseriesdemo[timeseriesdemo['anomalies'] == 1]




.. raw:: html

    <div>
    <style scoped>
        .dataframe tbody tr th:only-of-type {
            vertical-align: middle;
        }

        .dataframe tbody tr th {
            vertical-align: top;
        }

        .dataframe thead th {
            text-align: right;
        }
    </style>
    <table border="1" class="dataframe">
      <thead>
        <tr style="text-align: right;">
          <th></th>
          <th>TimeGenerated</th>
          <th>TotalBytesSent</th>
          <th>baseline</th>
          <th>score</th>
          <th>anomalies</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <th>299</th>
          <td>2019-05-13 17:00:00</td>
          <td>916767394</td>
          <td>662107538</td>
          <td>3.247957</td>
          <td>1</td>
        </tr>
        <tr>
          <th>399</th>
          <td>2019-05-17 21:00:00</td>
          <td>1555286702</td>
          <td>1212399509</td>
          <td>4.877577</td>
          <td>1</td>
        </tr>
        <tr>
          <th>599</th>
          <td>2019-05-26 05:00:00</td>
          <td>1768911488</td>
          <td>1391114419</td>
          <td>5.522387</td>
          <td>1</td>
        </tr>
      </tbody>
    </table>
    </div>



Time Series Anomalies Visualization
-----------------------------------

Time series anomalies once discovered, you can visualize with line chart
type to display outliers. Below we will see 2 types to visualize using msticpy function
``display_timeseries_anomalies()`` via Bokeh library as well as using
built-in KQL ``render``.

Using Bokeh Visualization Library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Documentation for display_timeseries_anomalies
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   display_timeseries_anomolies(
       data: pandas.core.frame.DataFrame,
       y: str = 'Total',
       time_column: str = 'TimeGenerated',
       anomalies_column: str = 'anomalies',
       source_columns: list = None,
       period: int = 30,
       **kwargs,
   ) -> <function figure at 0x7f0de9ae2598>
   Docstring:
    Display time series anomalies visualization.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame as a time series data set retreived from KQL time series functions
        dataframe will have columns as TimeGenerated, y, baseline, score, anomalies
    y : str, optional
        Name of column holding numeric values to plot against time series to determine anomolies
        (the default is 'Total')
    time_column : str, optional
        Name of the timestamp column
        (the default is 'TimeGenerated')
    anomalies_column : str, optional
        Name of the column holding binary status(1/0) for anomaly/benign
        (the default is 'anomolies')
    source_columns : list, optional
        List of default source columns to use in tooltips
        (the default is None)
    period : int, optional
        Period of the dataset for hourly-no of days, for daily-no of weeks.
        This is used to correctly calculate the plot height.
        (the default is 30)

    Other Parameters
    ----------------
    ref_time : datetime, optional
        Input reference line to display (the default is None)
    title : str, optional
        Title to display (the default is None)
    legend: str, optional
        Where to position the legend
        None, left, right or inline (default is None)
    yaxis : bool, optional
        Whether to show the yaxis and labels
    range_tool : bool, optional
        Show the the range slider tool (default is True)
    height : int, optional
        The height of the plot figure
        (the default is auto-calculated height)
    width : int, optional
        The width of the plot figure (the default is 900)
    xgrid : bool, optional
        Whether to show the xaxis grid (default is True)
    ygrid : bool, optional
        Whether to show the yaxis grid (default is False)
    color : list, optional
        List of colors to use in 3 plots as specified in order
        3 plots- line(observed), circle(baseline), circle_x/user specified(anomalies).
        (the default is ["navy", "green", "firebrick"])

    Returns
    -------
    figure
        The bokeh plot figure.

.. code:: ipython3

    display_timeseries_anomolies(data=timeseriesdemo, y= 'TotalBytesSent')



.. raw:: html


        <div class="bk-root">
            <a href="https://bokeh.org" target="_blank" class="bk-logo bk-logo-small bk-logo-notebook"></a>
            <span id="1001">Loading BokehJS ...</span>
        </div>



.. image:: _static/TimeSeriesAnomalieswithRangeTool.png





Exporting Plots as PNGs
^^^^^^^^^^^^^^^^^^^^^^^

To use bokeh.io image export functions you need selenium, phantomjs and
pillow installed:

``conda install -c bokeh selenium phantomjs pillow``

or

``pip install selenium pillow`` ``npm install -g phantomjs-prebuilt``

For phantomjs see https://phantomjs.org/download.html.

Once the prerequisites are installed you can create a plot and save the
return value to a variable. Then export the plot using ``export_png``
function.

.. code:: ipython3

    from bokeh.io import export_png
    from IPython.display import Image

    # Create a plot
    timeseries_anomaly_plot = display_timeseries_anomolies(data=timeseriesdemo, y= 'TotalBytesSent')

    # Export
    file_name = "plot.png"
    export_png(timeseries_anomaly_plot, filename=file_name)

    # Read it and show it
    display(Markdown(f"## Here is our saved plot: {file_name}"))
    Image(filename=file_name)



.. raw:: html


        <div class="bk-root">
            <a href="https://bokeh.org" target="_blank" class="bk-logo bk-logo-small bk-logo-notebook"></a>
            <span id="1407">Loading BokehJS ...</span>
        </div>



Here is our saved plot: plot.png
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


.. image:: _static/TimeSeriesAnomaliesExport.png


Using Built-in KQL render operator
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Render operator instructs the user agent to render the results of the
query in a particular way. In this case, we are using timechart which
will display linegraph.

**KQL Reference Documentation:** -
`render <https://docs.microsoft.com/en-us/azure/kusto/query/renderoperator?pivots=azuremonitor>`__

.. code:: ipython3

    timechartquery = """
    let TimeSeriesData = PaloAltoTimeSeriesDemo_CL
    | extend TimeGenerated = todatetime(EventTime_s), TotalBytesSent = todouble(TotalBytesSent_s)
    | summarize TimeGenerated=make_list(TimeGenerated, 10000),TotalBytesSent=make_list(TotalBytesSent, 10000) by deviceVendor_s
    | project TimeGenerated, TotalBytesSent;
    TimeSeriesData
    | extend (baseline,seasonal,trend,residual) = series_decompose(TotalBytesSent)
    | mv-expand TotalBytesSent to typeof(double), TimeGenerated to typeof(datetime), baseline to typeof(long), seasonal to typeof(long), trend to typeof(long), residual to typeof(long)
    | project TimeGenerated, TotalBytesSent, baseline
    | render timechart with (title="Palo Alto Outbound Data Transfer Time Series decomposition")
    """
    %kql -query timechartquery


.. image:: _static/TimeSeriesKQLPlotly.PNG