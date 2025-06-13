********************
Modelling frameworks
********************

Quickstart
==========
.. CW this paragraph as start of Chapter, not section

In addition to spatial functions a support frame is required to build spatio-temporal models. This section introduces two frameworks that ease the development of static and dynamic models.

The script :ref:`below <runoff.py>` is the Python version of the hydrological runoff model shown in the demo of the PCRaster distribution. To run the script change to the demo directory (demo/deterministic) and execute


.. highlight:: bash

::

  python runoff.py

..       <title>Python script <filename>runoff.py</filename> specifying a spatio-temporal model using the dynamic modelling framework.</title>
..       <tgroup cols="1"><tbody><row><entry>-->
..               <example id="list:runoff.py">
..                 <title>
..                   Python script <filename>runoff.py</filename> specifying a spatio-temporal model using the dynamic modelling framework.
..                 </title>

.. _runoff.py:

.. literalinclude:: ../../data/demo/python_modelling_framework/deterministic/runoff.py
   :language: python
   :linenos:

Deterministic Modelling
=======================

Static Modelling Framework
--------------------------
This section introduces to the usage of the static modelling framework. A static model is described by:

.. math::

   Z = f(Z, I, P)

with :math:`Z`, the model state variables; :math:`I`, inputs; :math:`P`, parameter; and :math:`f` defining the model structure. The static model framework is used to build models without temporal dependencies, like calculating distances between a number of gauging stations.

Static model template
^^^^^^^^^^^^^^^^^^^^^
The following script shows the minimal user class that fulfills the requirements for the static framework:

.. code-block:: python

   # userModel.py
   import pcraster.framework as pcrfw

   class UserModel(pcrfw.StaticModel):
       def __init__(self):
           pcrfw.StaticModel.__init__(self)

       def initial(self):
           pass

In the class of the user model the following method must be implemented:

.. py:method:: initial()

   This method contains the static section of the user model.

The model class can be executed with the static framework as follows:

.. code-block:: python

   # runScript.py
   import pcraster.framework as pcrfw
   import userModel

   myModel = userModel.UserModel()
   staticModel = pcrfw.StaticFramework(myModel)
   staticModel.run()

To run the model execute

.. highlight:: bash

::

  python runScript.py

The script `runScript.py` creates an instance of the user model which is passed to the static framework afterwards. `staticModel.run()` executes the initial section of the user model.

.. CW add: Note that this minimal setup does not do anything;
.. <function>pass</function> is the Python stub for not doing anything. In reality there wil be modelling code at the spots of the <function>pass</function> statement.

Example
^^^^^^^
The following example shows the static version of the demo script. PCRaster operations can be used in the same way as in scripts without the modelling framework:

..           <title>
..             Static version of the demo script.
..           </title>

.. literalinclude:: ../../data/demo/python_modelling_framework/deterministic/runoffStatic.py
   :language: python
   :linenos:

Setting the map attributes (e.g. number of rows and columns and cellsize) is done by using `setclone` in the constructor of the model class.

PCRaster operations can have data from disk as input arguments, as is done in the `spreadzone` operation.

Note that the framework provides an additional report operation (`self.report`) whose behavior is dependent on the method in which it is used. It writes the data to disk with a filename conforming to the PCRaster conventions generated from the second argument, i.e. appending a ".map" suffix for the static framework (the name of the local drain direction map will become "ldd.map") and appending a time step when used in the dynamic framework. Storing data with a specific name or at a specific location is done using `report` instead of `self.report`.

..       <!--       <sect3>
..         <title>Methods provided by the framework</title>
..         <para>The following methods are added by the framework to the user class: [report not
..           added, but has different report locations...]
..
..         </para>
..         <variablelist>
..           <varlistentry>
..             <term><function>self.report(map, filename)</function><indexterm>
..                 <primary>report</primary>
..                 <secondary>static framework</secondary>
..               </indexterm></term>
..             <listitem>
..               <para>This methods writes the object map[this must be specified ] to disk</para>
..             </listitem>
..           </varlistentry>
..         </variablelist>
..       </sect3>-->

Dynamic Modelling Framework
---------------------------
This section describes the usage of the dynamic modelling framework. In addition to spatial processes dynamic models include a temporal component. Simulating dynamic behaviour is done by iterating a dynamic section over a set of timesteps.

The state of a model variable at time :math:`t` is defined by its state at :math:`t-1` and a function :math:`f` (:ref:`Karssenberg2005a <Karssenberg2005a>`):

.. math::

  Z_{1..m}(t) = f(Z_{1..m}(t-1), I_{1..n}(t), P_{1..l})

The model state variables :math:`Z_{1..m}` belong to coupled processes and have feedback in time. :math:`I_{1..n}` denote the inputs to the model, :math:`P_{1..l}` are model parameters, and :math:`f` transfers the model state from time step :math:`t-1` to :math:`t`.

The dynamic modelling framework executes :math:`f` using the following scheme (in pseudo code):

.. highlight:: none

::

   initial()

   for each timestep:
     dynamic()

Dynamic model template
^^^^^^^^^^^^^^^^^^^^^^
The following script shows the minimal user class that fulfills the requirements for the dynamic framework:

.. code-block:: python

   # userModel.py
   import pcraster.framework as pcrfw


   class UserModel(pcrfw.DynamicModel):
       def __init__(self):
           pcrfw.DynamicModel.__init__(self)

       def initial(self):
           pass

       def dynamic(self):
           pass

In the class of the user model the following methods must be implemented:

.. py:method:: initial()
   :noindex:

   This method contains the code to initialise variables used in the model.

.. py:method:: dynamic()

   This method contains the implementation of the dynamic section of the user model.

Applying the model to the dynamic framework is done by:

.. code-block:: python

   # runScript.py

   import pcraster.framework as pcrfw
   import userModel

   myModel = userModel.UserModel()
   dynModel = pcrfw.DynamicFramework(myModel, 50)
   dynModel.run()

To run the model execute

.. highlight:: bash

::

  python runScript.py

The script `runScript.py` creates an instance of the user model which is passed to the dynamic framework. The number of time steps is given as second argument to the framework constructor.

Example
^^^^^^^
A script for a dynamic model is given in the :ref:`quick start section <runoff.py>`. The model contains two main sections:

The `initial` section contains operations to initialise the state of the model at time step 0. Operations included in this section are executed once.

The `dynamic` section contains the operations that are executed consecutively each time step. Results of a previous time step can be used as input for the current time step. The dynamic section is executed a specified number of timesteps: 28 times in the demo script.

The initial section of the demo script is the same as in the static version. The dynamic section holds the operations for in- and output with temporal dependencies and the model processes. For time series input data the `timeinputscalar` assigns precipitation data for each time step to the `surfaceWater` variable. In the case that `rain0000.001` to `rain0000.028` hold the rainfall for each timestep instead you can replace the `timeinputscalar` operation by `surfaceWater = self.readmap("rain")`.

Output data is now reported as a stack of maps to disk. The function `self.report` will store the runoff with filenames `logrunof.001` up to `logrunof.028`.

For additional operations that can be used for example in conditional expressions like `self.currentTimestep` we refer to the code reference for this topic.

..       <!--
..           CW: I really  like an example with feedback here, such
..              as SnowMelt model
..         -->
..       <!--       <sect3>
..         <title>Methods provided by the framework</title>
..         <para>The following methods are added by the framework to the user class:
..
..         </para>
..         <variablelist>
..           <varlistentry>
..             <term><function>self.nrTimeSteps()</function></term>
..             <listitem>
..               <para>returns the total number of timesteps</para>
..             </listitem>
..           </varlistentry>
..           <varlistentry>
..             <term><function>self.currentTimestep()</function></term>
..             <listitem>
..               <para>returns the current timestep</para>
..             </listitem>
..           </varlistentry>
..           <varlistentry>
..             <term>self.report()<indexterm>
..                 <primary>report</primary>
..                 <secondary>dynamic framework</secondary>
..               </indexterm></term>
..             <listitem>
..               <para>reports to disk</para>
..             </listitem>
..           </varlistentry>
..           <varlistentry>
..             <term><function>self.timeSteps()</function></term>
..             <listitem>
..               <para>returns a list of timesteps</para>
..             </listitem>
..           </varlistentry>
..           <varlistentry>
..             <term>more...</term>
..             <listitem>
..               <para/>
..             </listitem>
..           </varlistentry>
..         </variablelist>
..       </sect3>-->

Stochastic Modelling and data assimilation
==========================================
In the case that a model includes probabilistic rules or inputs variables and parameters that are given as spatial probability distributions, the model becomes stochastic (:ref:`Karssenberg2005b <Karssenberg2005b>`. The aim of stochastic modelling is to derive the probability distributions, which is done in the framework by Monte Carlo simulation.

The framework provides three different methods to support stochastic modelling and data assimilation: Monte Carlo simulation (e.g. :ref:`Doucet2000 <Doucet2000>`, :ref:`Doucet2001 <Doucet2001>`), particle filter (e.g. :ref:`Xiong2006 <Xiong2006>`, :ref:`Weerts2006 <Weerts2006>`, :ref:`Arulampalam2002 <Arulampalam2002>`) and the Ensemble Kalman filter (e.g. :ref:`Evensen1994 <Evensen1994>`, :ref:`Simon2006 <Simon2006>`).

Monte Carlo simulations
-----------------------
Monte Carlo simulations solve for a large number of samples the function :math:`f` and compute statistics on the ensemble results. The framework supports this scheme by executing the following methods (in pseudo code):

.. highlight:: none

::

   premcloop()

   for each sample:
     initial()
     if dynamic model:
       for each timestep:
         dynamic()

   postmcloop()

The following additional methods must be implemented to use the framework in Monte Carlo mode:

.. py:method:: premcloop()

   The premcloop can be used to calculate input parameters or variables that are both constant and deterministic. It is executed once at the beginning of the model run, the calculate variables can be used in all samples and time steps.

.. py:method:: postmcloop()

   The postmcloop is executed after the last sample run is finished. It is used to calculate statistics of the ensemble, like variance or quantiles.

The `initial` and `dynamic` sections (the latter in case of a dynamic model for each time step) are executed for each Monte Carlo sample.

The framework generates samples directories named 1, 2, 3,..., N, with N the number of Monte Carlo samples. The methods `self.readmap()` and `self.report()` now read and store the data to and from the corresponding sample directory.

Static models
^^^^^^^^^^^^^
The Python script :ref:`below <montecarlo.py>` shows a static model which is executed within the Monte Carlo framework. The model simulates vegetation growth; 100 realisations are executed (:ref:`Karssenberg2005b <Karssenberg2005b>`).

.. _montecarlo.py:

.. Python script specifying a static model executed in the Monte Carlo framework.

.. literalinclude:: montecarloStatic.py
   :language: python
   :linenos:

First, maps are created containing for each cell the time (years) needed for the plant to spread 1 m. The value and the error associated with this input parameter depend on the soil type. By using the function `mapnormal` each sample will generate an independent realisation of the input parameter `peatYears` and `otherYears`. The information is used to calculate a total spreading time map from the locations occupied with the plant on `distr.map`. Finally a Boolean map is generated containing all cells colonized within 50 years.

Dynamic models
^^^^^^^^^^^^^^
The Python script :ref:`below <montecarlodyn.py>` shows a dynamic model which is executed within the Monte Carlo framework. The model simulates snow thickness and discharge for 180 time steps (:ref:`Karssenberg2009 <Karssenberg2009>`). A number of 10 realisations is executed.

.. _montecarlodyn.py:

..         <title>
..           Python script <filename>montecarlodyn.py</filename> specifying a dynamic model executed in the Monte Carlo framework.
..         </title>

.. literalinclude:: ../../data/demo/python_modelling_framework/stochastic/montecarlo.py
   :language: python
   :linenos:

To run the model execute

.. highlight:: bash

::

  python montecarlo.py

In the `premcloop`, the local drain direction map is created from the digital elevation map `dem.map`. As the local drain direction map is used in the initial and dynamic section later on it is defined as a member variable of the snowModel class. If maps are reported in the `premcloop` they are written into the current working directory.

The `initial` section initialises for each realisation the state variables snow with an original value of zero. Also a realisation of the lapse rate is made from a probability distribution :math:`0.005 + norm(0, 0.001)`. Each lapse rate is stored with `self.report` in the corresponding sample subdirectory.

The `dynamic` section calculates the snow height and the discharge. The temperature and precipitation values are obtained from disk with the `self.readDeterministic` operation. Random noise is added to the deterministic precipitation values in order to create independent realisations. The precipitation is diminished by the potential snowfall, which builds up the snow depth. Runoff and snowmelt compose the discharge in the catchment.

The `postmcloop` calculates statistics over all ensemble members. For snow and discharge, average and variance values are calculated. Furthermore the percentiles are calculated for both variables. The resulting maps of the `postmcloop` calculations are written to the current working directory.

Particle filter
---------------
The particle filter is a method to improve the model predictions. Observed values are hereby used at specific time steps to determine the best performing samples. The prediction performance of the ensemble is improved by continuing better performing and omitting bad samples.

The particle filter approximates the posterior probability density function from the weights of the Monte Carlo samples :ref:`Karssenberg2009 <Karssenberg2009>`:

.. math::

  p(x_t \mid Y_t) \approx \sum_{n=1}^N p_t^{(n)} \delta (x_t - x_t^{(n)})

with :math:`\delta` the Dirac delta function, :math:`Y_t` the past and current observations at time :math:`t` and :math:`x_t` a vector of model components for which observation are available.

For Gaussian measurement error the weights are proportional to :ref:`Simon2006 <Simon2006>`:

.. math::
   :label: gauss

   a_t^{(n)} = exp(-[y_t - h_t(x_t^{(n)})]^T R_t^{-1} [y_t - h_t(x_t^{(n)})] / 2)

with :math:`R_t` the covariance matrix of the measurement error and :math:`h_t` the measurement operator.

The weight of a sample is calculated by a normalisation of :math:`a_t^{(n)}`:

.. math::

   p_t^{(n)} = a_t^{(n)} / \sum_{j=1}^N a_t^{(j)}

The particle filter framework executes the following methods using the scheme:

.. highlight:: none

::

   premcloop()

   for each filter period:
     for each sample:
       if first period:
         initial()
       else:
         resume()
       for each timestep in filter period:
         dynamic()
       if not last filter period:
         suspend()
       updateWeight()

   postmcloop()

The following additional methods must be implemented to use the framework:

.. py:method:: suspend()

   The suspend section is executed after the time step precedent to a a filter timestep and used to store the state variables. This can be achieved with the `self.report()` method.

.. py:method:: resume()

   The resume section is executed before the first time step of a filter period and intended to re-initialise the model after a filter time step. State variables can be obtained with the `self.readmap()` method.

.. py:method:: updateWeight()

   The updateWeight method is executed at the filter moment and used to retrieve the weight of each sample. The method must return a single floating point value (i.e. the :math:`a_t^{(n)}`).

Like in the Monte Carlo framework each sample output will be stored into a corresponding sample directory. Each sample directory contains a `stateVar` subdirectory that is used to store the state variables of the model. State variables not holding PCRaster data types must be stored into this directory by the user.

Two different algorithms are implemented in the filter framework. Sequential Importance Resampling and Residual Resampling (see e.g. :ref:`Weerts2006 <Weerts2006>`) can be chosen as selection scheme by using the adequate framework class. In Sequential Importance Resampling, a cumulative distribution function is constructed from the sample weights :math:`p_t^{(n)}`. From this distribution N samples are drawn with replacement from a uniform distribution between 0 and 1.

In Residual Resampling, in the first step samples are cloned a number of times equal to :math:`k_t^{(n)} = floor(p_t^{(n)}N)` with N number of samples and :math:`floor` an operation rounding the value to the nearest integer. In a second step, the residual weights :math:`r_t^{(n)}` are calculated according to:

.. math::

   r_t^{(n)} = {{p_t^{(n)}N - k_t^{(n)}} \over {N - \sum_{n=1}^N k_t^{(n)}}}

and used to construct a cumulative distribution function. From this distribution a number of additional samples is drawn until a number of N samples is reached :ref:`Karssenberg2009 <Karssenberg2009>`.

For each filter time step a comma separated file holding sample statistics is written to the current working directory. For each sample it contains its normalised weight, the cumulative weight up to that sample and the number of clones for that sample. A zero  indicates that the sample is not continued. Furthermore a graphviz_ input file holding the sample choice is generated.

.. _graphviz: http://www.graphviz.org

Example
^^^^^^^
The script :ref:`below <particlefilter.py>` shows a dynamic model which is executed within the particle filter framework.  The overall runtime of the model still accounts to 180 time steps, 10 realisations are executed. As three filter moments are chosen at the timesteps 70, 100 and 150 four periods in total are executed: from timestep 1-70, 71-100, 101-150 and 151-180.

.. _particlefilter.py:

..    Python script <filename>particlefilter.py</filename> specifying a model executed in the particle filter framework.

.. literalinclude:: ../../data/demo/python_modelling_framework/stochastic/particlefilter.py
   :language: python
   :linenos:

Compared to the script in :ref:`montecarlo.py <montecarlo.py>` the three methods `suspend`, `resume` and `updateWeight` are added. The sections `initial`, `dynamic`, `premcloop` and `postmcloop` remain identical to the Monte Carlo version.

The state variables of the model are the  snow height and the lapse rate. These variables are stored in the `suspend()` section with `self.reportState()` into the state variable directory. They are either cloned or replaced by the filter, or continued in the following filter period.

In the `resume()` method the lapse rate will now be set to either the same value as before the filter moment or to a new value cloned from another sample. The same procedure applies to the snow state variable. As the value of `self.temperatureCorrection` is dependent on the lapse rate it has to be re-initialised too.

To calculate the weigtht of a sample the model implements in `updateWeight` the equation :eq:`gauss` (:ref:`Karssenberg2009 <Karssenberg2009>`).

For five meteorological stations in different elevation zones the average snow height values are compared. For the modelled data the zonal values are calculated with `areaaverage`, the observation values are read from disk. As the `observedAverageMap` contains missing values except at the measurement locations the `maptotal` operation yields the sum over the five elevation zones for the exponent of the weight calculation. The sample weight is afterwards extracted as individual floating point value.

Ensemble Kalman filter
----------------------
The Ensemble Kalman Filter is a Monte Carlo approximation of the Kalman filter :ref:`Evensen2003 <Evensen2003>`. Contrary to the cloning of the particle filter the ensemble Kalman filter modifies the state variables according to:

.. math::
   :label: kalman

   n_t^{(n),+} = n_t^{(n),0} + P_t^0 H^T (H_t P_t^0 H_t^T + R_t)^{-1} (y_t^{(n)} - H_t x_t^{(n),0})

for each sample n, where :math:`x_t^{(n)}` is a vector containing a realisation n at update moment :math:`t` of model components for which observations are available. The superscript :math:`0` indicates the prior state vector and superscript :math:`+` indicates the posterior state vector calculated by the update. :math:`P_t^0` is the ensemble covariance matrix. :math:`y_t^{(n)}` is a realisation of the :math:`y_t` vector holding the observations. :math:`R_t` is the error covariance matrix and :math:`H_t` the measurement operator (:ref:`Evensen2003<Evensen2003>`, :ref:`Karssenberg2009 <Karssenberg2009>`).

The execution scheme is similar to the one of the particle filter:

.. highlight:: none

::

   premcloop()

   for each filter period:
     for each sample:
       if first period:
         initial()
       else:
         resume()
       for each timestep in filter period:
         dynamic()
       setState()
     setObservations()

  postmcloop()

The user has to implement the `setState`, `setObservations` and the `resume` methods in the model class. As state variables (and eventually parameters) are modified the `setState` method now needs to return a vector (i.e. the :math:`x_t^{(n),0}` in equation :eq:`kalman`) holding the values instead of an individual value. In the `resume` section the updated values (i.e. the :math:`x_t^{(n),+}` in equation :eq:`kalman`) can be obtained with the `getStateVector` method.

For each update moment the user needs to provide the observed values :math:`y_t` to the Ensemble Kalman framework with the `setObservations` method. The associated measurement error covariance matrix is set with `setObservedMatrices`. The measurement operator :math:`H_t` can be set with the `setMeasurementOperator` method.

Example
^^^^^^^
The script :ref:`below <kalmanfilter.py>` shows again the snow model which is now executed within the Ensemble Kalman filter framework.  The overall runtime of the model still accounts to 180 timesteps, 10 realisations are executed. Again three filter moments are chosen at the timesteps 70, 100 and 150.

.. _kalmanfilter.py:

..  Python script <filename>kalmanfilter.py</filename> specifying a model executed in the ensemble Kalman filter framework.</title><?dbfo keep-together="auto" ?>

.. literalinclude:: ../../data/demo/python_modelling_framework/stochastic/kalmanfilter.py
   :language: python
   :linenos:

In the `setState` section the average snow pack is calculated from the sample snow cover map. The map holding the average values is stored in the sample subdirectory in order to avoid recalculating the values in the `resume` section. The average value for each zone is extracted as an individual value and inserted into a numpy matrix. This matrix is returned to the framework.

In the `resume` section the array returned by the `getStateVector` now holds the updated state variables. In the following the correction factor for the snow values is calculated as the difference between the modelled averaged snow heights and the average snow heights returned by the Ensemble Kalman filter. For each zone this correction factor is applied to the snow pack cell values in order to obtain the new snow pack map.

