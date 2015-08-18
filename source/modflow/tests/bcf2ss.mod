# bcf2ss


areamap
  bcf2ss_clone.map;

timer
  1 2 1;

initial
  object bcf2ss = pcraster_modflow::initialise();

  report l1.pcrmap = if(bcf2ss_clone.map then scalar(-50.0));
  report l1_top.pcrmap = if(bcf2ss_clone.map then scalar(0.0));
  report l2_top.pcrmap = if(bcf2ss_clone.map then scalar(50.0));
  report l3_top.pcrmap = if(bcf2ss_clone.map then scalar(150.0));

  res = bcf2ss::createBottomLayer(l1.pcrmap, l1_top.pcrmap);
  res = bcf2ss::addConfinedLayer(l2_top.pcrmap);
  res = bcf2ss::addLayer(l3_top.pcrmap);

  report ib_1.pcrmap = if(bcf2ss_clone.map then nominal(1));
  report ib_3.pcrmap = if(bcf2ss_clone.map then nominal(0));
  report head.pcrmap = if(bcf2ss_clone.map then scalar(0));

  res = bcf2ss::setBoundary(ib_1.pcrmap, 1);
  res = bcf2ss::setBoundary(ib_3.pcrmap, 3);
  res = bcf2ss::setInitialHead(head.pcrmap, 1);
  res = bcf2ss::setInitialHead(head.pcrmap, 3);

  report hcond.pcrmap = if(bcf2ss_clone.map then scalar(10));
  report hcond3.pcrmap = if(bcf2ss_clone.map then scalar(10));

  # just make up some vcond value
  report vcond1.pcrmap = if(bcf2ss_clone.map then scalar(25));
  report vcond2.pcrmap = if(bcf2ss_clone.map then scalar(0.05));
  report vcond3.pcrmap = if(bcf2ss_clone.map then scalar(-50));

  res = bcf2ss::setConductivity(0, hcond.pcrmap, vcond1.pcrmap, 1);
  res = bcf2ss::setConductivity(0, hcond.pcrmap, vcond2.pcrmap, 2);
  res = bcf2ss::setConductivity(1, hcond3.pcrmap, vcond3.pcrmap, 3);
  res = bcf2ss::setDryHead(777.77);
  res = bcf2ss::setNoFlowHead(999.99);

  res = bcf2ss::setWettingParameter(1, 1, 0);
  res = bcf2ss::setWetting(bcf2ss_wet.map, 3);

  report rch.pcrmap = if(bcf2ss_clone.map then scalar(0.004));
  res = bcf2ss::setRecharge(rch.pcrmap, 3);

  report rh.pcrmap = if(bcf2ss_riv.map then scalar(0) else 0);
  report rb.pcrmap = if(bcf2ss_riv.map then scalar(-5) else 0);
  report rc.pcrmap = if(bcf2ss_riv.map then scalar(10000) else 0);

  res = bcf2ss::setRiver(rh.pcrmap, rb.pcrmap, rc.pcrmap, 1);

  res = bcf2ss::setDISParameter(4, 0, 1, 1, 1, 1);

  res = bcf2ss::setPCG(40, 20, 1, 0.001, 1000.0, 1, 2, 1);

  # time step 1
  res = bcf2ss::run();
  report hTwo_1.map = bcf2ss::getHeads(1);
  report hOne_1.map = bcf2ss::getHeads(3);
  report rTwo_1.map = bcf2ss::getRecharge(1);
  report rOne_1.map = bcf2ss::getRecharge(3);
  report riTwo_1.map = bcf2ss::getRiverLeakage(1);
  report riOne_1.map = bcf2ss::getRiverLeakage(3);

  report chTwo_1.map = bcf2ss::getConstantHead(1);
  report chOne_1.map = bcf2ss::getConstantHead(3);
  report ffTwo_1.map = bcf2ss::getFrontFace(1);
  report ffOne_1.map = bcf2ss::getFrontFace(3);
  report rfTwo_1.map = bcf2ss::getRightFace(1);
  report rfOne_1.map = bcf2ss::getRightFace(3);
  report lfOne_1.map = bcf2ss::getLowerFace(3);

  # time step 2
  res = bcf2ss::setWell(bcf2ss_wel.map, 1);
  res = bcf2ss::run();
  report hTwo_2.map = bcf2ss::getHeads(1);
  report hOne_2.map = bcf2ss::getHeads(3);
  report rTwo_2.map = bcf2ss::getRecharge(1);
  report rOne_2.map = bcf2ss::getRecharge(3);
  report riTwo_2.map = bcf2ss::getRiverLeakage(1);
  report riOne_2.map = bcf2ss::getRiverLeakage(3);

  report chTwo_2.map = bcf2ss::getConstantHead(1);
  report chOne_2.map = bcf2ss::getConstantHead(3);
  report ffTwo_2.map = bcf2ss::getFrontFace(1);
  report ffOne_2.map = bcf2ss::getFrontFace(3);
  report rfTwo_2.map = bcf2ss::getRightFace(1);
  report rfOne_2.map = bcf2ss::getRightFace(3);
  report lfOne_2.map = bcf2ss::getLowerFace(3);
