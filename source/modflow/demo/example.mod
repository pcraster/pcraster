# This example script demonstrates the use of the
# PCRasterModflow extension. It recreates the example
# problem given in the user guide of MODFLOW-2000
# (Open file report 00-92)
#
# To run the script use
# pcrcalc -f example.mod

binding
  elev = elev.map;
  bottom = bottom.map;
  l1_top = l1_top.map;
  l2_top = l2_top.map;
  l3_top = l3_top.map;
  l4_top = l4_top.map;
  l5_bound = l5_bound.map;
  l3_bound = l3_bound.map;
  l1_bound = l1_bound.map;
  head = head.map;
  hcond_l5 = hcond_l5.map;
  hcond_l3 = hcond_l3.map;
  hcond_l1 = hcond_l1.map;
  vcond_l1=vcond_l1.map;
  vcond_l2=vcond_l2.map;
  vcond_l3=vcond_l3.map;
  vcond_l4=vcond_l4.map;
  vcond_l5=vcond_l5.map;
  rch = rch.map;
  drain_cond = drain_cond.map;
  drain_elev = drain_elev.map;
  wel_l1 = well_l1.map;
  wel_l3 = well_l3.map;
  wel_l5 = well_l5.map;

areamap
  clone.map;

timer
  1 1 1;

initial
  object mf = pcraster_modflow::initialise();

  # do not use result maps as input in oncomming operations!

  # grid specification
  res = mf::createBottomLayer(bottom, l1_top);
  res = mf::addConfinedLayer(l2_top);
  res = mf::addLayer(l3_top);
  res = mf::addConfinedLayer(l4_top);
  res = mf::addLayer(elev);

  # simulation parameter
  res = mf::setDISParameter(1, 0, 86400, 1, 1, 1);

  # adding the boundary conditions to the MODFLOW model
  res = mf::setBoundary(l1_bound, 1);
  res = mf::setBoundary(l3_bound, 3);
  res = mf::setBoundary(l5_bound, 5);

  # adding the initial values to the MODFLOW model
  res = mf::setInitialHead(head, 1);
  res = mf::setInitialHead(head, 3);
  res = mf::setInitialHead(head, 5);

  # adding the conductivities to the MODFLOW model
  res = mf::setConductivity(0, hcond_l1, vcond_l1, 1);
  res = mf::setConductivity(0, hcond_l1, vcond_l2, 2);
  res = mf::setConductivity(0, hcond_l3, vcond_l3, 3);
  res = mf::setConductivity(0, hcond_l3, vcond_l4, 4);
  res = mf::setConductivity(1, hcond_l5, vcond_l5, 5);

  # solver package
  res = mf::setSIP(50, 5, 1.0, 0.001, 0, 0.001);

  res = mf::setDryHead(1E30);

  # adding the drain
  res = mf::setDrain(drain_elev, drain_cond, 5);

  # specifying the recharge
  res = mf::setRecharge(rch, 1);

  # adding well 
  res = mf::setWell(wel_l1, 1);
  res = mf::setWell(wel_l3, 3);
  res = mf::setWell(wel_l5, 5);

  # execute MODFLOW modelling engine
  res = mf::run();

  # retrieve head values
  report hFive.map = mf::getHeads(5);
  report hThree.map = mf::getHeads(3);
  report hOne.map = mf::getHeads(1);

  # retrieve drain output
  report dFive.map = mf::getDrain(5);

  # retrieve recharge output
  report rFive.map = mf::getRecharge(5);

  # retrieve BCF outputs
  report c5.map = mf::getConstantHead(5);
  report c3.map = mf::getConstantHead(3);
  report c1.map = mf::getConstantHead(1);

  report fr5.map = mf::getRightFace(5);
  report fr3.map = mf::getRightFace(3);
  report fr1.map = mf::getRightFace(1);

  report ff5.map = mf::getFrontFace(5);
  report ff3.map = mf::getFrontFace(3);
  report ff1.map = mf::getFrontFace(1);

  report fl5.map = mf::getLowerFace(5);
  report fl3.map= mf::getLowerFace(3);
