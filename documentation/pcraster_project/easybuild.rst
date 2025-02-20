Installing PCRaster with EasyBuild
==================================

In case you work on a system where environment modules and `EasyBuild <https://easybuild.io/>`_ are available you can simply compile your own PCRaster package.
First, download a `configuration <https://github.com/pcraster/pcraster/tree/master/environment/easybuild>`_ file.
Then, build PCRaster and potentially the required dependencies e.g. with

.. code-block:: console

   eb PCRaster-4.3.3-GCCcore-10.3.0-Python-3.9.5.eb --robot

After successful completion you can add PCRaster to your environment

.. code-block:: console

   module load PCRaster/4.3.3-GCCcore-10.3.0-Python-3.9.5


You might need to adapt version numbers depending on the EasyBuild version you use but the script should give a good start.
