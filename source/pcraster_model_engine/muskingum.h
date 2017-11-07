/*
<Operation name='muskingumtimestep'
           exec='Direct'>
 <Result contextName='flowRateTimestep'>
    <!-- volume/timestep -->
   <Field spatial='Yes'>&VS_S;</Field></Result>
 <Input contextName='Ldd'><Field spatial='Yes'>&VS_L;</Field></Input>
 <Input contextName='prevFlowRateTimestep'>
    <!-- volume/timestep -->
    <Field spatial='Yes'>&VS_S;</Field></Input>
 <Input contextName='prevInflowTimestep'>
    <!-- volume/sec -->
    <Field spatial='Yes'>&VS_S;</Field></Input>
 <Input contextName='inflowTimestep'>
    <!-- volume/timestep  -->
    <Field spatial='Yes'>&VS_S;</Field></Input>
 <Input contextName='KTimestepPerUnitSegmentLength'>
    <!-- timestep > 0 -->
    <Field>&VS_S;</Field></Input>
 <Input contextName='x'>
    <!-- interval in [0,1] -->
    <Field>&VS_S;</Field></Input>
 <Input contextName='segmentLength'>
   <!-- L -->
    <Field>&VS_S;</Field></Input>
 <Input contextName='nrTimeSlices'>
    <Field>&VS_NO;</Field></Input>
</Operation>
*/

// /home/cees/tmp/musPas/MuskingumCodeWD4.pas

//needs #define DEBUG_DEVELOP 1 on top of file
// #define MG_DEBUG(x) PRINT_VAR(x)
#define MG_DEBUG(x)
void calc::Muskingum::exec(
    RunTimeEnv* rte,
    const Operator& op,
    size_t nrArgs) const
{

  struct MGVisitor : public TimeSliceVisitor
  {
     void initPerCatchmentSlice(CurrentSliceInfo const& csi)
     {
       setCurrentSliceInfo(csi);
       //BOOST_CHECK_CLOSE(csi.sliceInSecs,3600.0, 0.0001);
     }
     void  finishVertexBeforeAllSlices(size_t v) {
       IterationFlowRateMap[v]=0;
       IterationPrevFlowRateMap[v]=d_prevFlowRate[v] * d_csi.sliceInSecs;
     }

     void     initVertexBeforeSlice(size_t v) {
       UpstreamSumIterationFlowRateMap[v]=0.0;
       UpstreamSumIterationPrevFlowRateMap[v]=0.0;
     }

     void visitEdge        (size_t up, size_t down) {
       // each iteration
       // send calculated flux to down
       com::inplace_add(UpstreamSumIterationFlowRateMap[down],IterationFlowRateMap[up]);
       com::inplace_add(UpstreamSumIterationPrevFlowRateMap[down],IterationPrevFlowRateMap[up]);
     }

     void finishVertex(size_t v) // each iteration
     {
       if (oneInputIsMV(v)|pcr::isMV(UpstreamSumIterationFlowRateMap[v])
                |pcr::isMV(UpstreamSumIterationPrevFlowRateMap[v]))
       {
         pcr::setMV(MyResultMap[v]);
         pcr::setMV(IterationFlowRateMap[v]);
         return;
       }

       // I = bovenstroomsesommatie(O) + Inflow
       double Inflow = UpstreamSumIterationFlowRateMap[v] +
          (d_inflow[v] *  d_csi.sliceInSecs);
       MG_DEBUG(Inflow);

       double InflowPrev = UpstreamSumIterationPrevFlowRateMap[v] +
           (d_prevInflow[v] *  d_csi.sliceInSecs);
       MG_DEBUG(InflowPrev);

       double FlowRatePrev =IterationPrevFlowRateMap[v]; //  m3/timestep


       double K = d_k[v] * d_segmentLength[v] * d_csi.sliceInSecs;  // My_K_Map in seconds
       double x = d_x[v];

       // BOOST_CHECK_CLOSE((double)kThisTimeSlice(v), (2.3*3600),0.0001);
       // BOOST_CHECK_CLOSE((double)x_(v), 0.15,0.0001);
       double C1 = (1+2*K*x)/(1+2*K*(1-x));
       MG_DEBUG(C1);
       double C2 = (1-2*K*x)/(1+2*K*(1-x));
       MG_DEBUG(C2);
       double C3 = (-1+2*K*(1-x))/(1+2*K*(1-x));
       MG_DEBUG(C3);


       DEVELOP_PRECOND(((C1+C2+C3)<1.00005) && ((C1+C2+C3)>0.99995));

       // fflush(0);
       // std::cerr << "CWInd " << v <<
       // " Inflow: " << Inflow << " InflowPrev: " << InflowPrev
       //  << " FlowRatePrev: " << FlowRatePrev
       //  << " UpsSumIterPrvFlwRate: "
       //  << UpstreamSumIterationPrevFlowRateMap[v]
       //  << std::endl;

       double FlowRate = (C1 * InflowPrev + C2*Inflow + C3*FlowRatePrev);
       IterationFlowRateMap[v] =FlowRate;  // m3/timestep
       MyResultMap[v]=(float)(MyResultMap[v]+FlowRate/d_csi.sliceInSecs);  // m3/sec
       MG_DEBUG(MyResultMap[v]);

       // NA alle visits
       // IterationPrevFlowRateMap[v]=IterationFlowRateMap[v]; // m3/timestep

     }

     void finishVertex2(size_t v) // each iteration
     {
       IterationPrevFlowRateMap[v]=IterationFlowRateMap[v]; // m3/timestep
     }

     void finishVertexAfterAllSlices(size_t v)
     {
      if (!pcr::isMV(MyResultMap[v]))
          MyResultMap[v] /=  d_csi.nrTimeSlices;
     }

     // Result
     float*               MyResultMap; // [m3/sec]

     std::vector<double>  IterationFlowRateMap;
     std::vector<double>  IterationPrevFlowRateMap;

     // intermediates
     std::vector<double>  UpstreamSumIterationFlowRateMap;
     std::vector<double>  UpstreamSumIterationPrevFlowRateMap;


     // read only inputs, exactly as argument of function
     const VField<float> d_prevFlowRate; // m3/timestep
     const VField<float> d_prevInflow;   // m3/timestep
     const VField<float> d_inflow;       // m3/timestep
     const VField<float> d_k;            // timestep
     const VField<float> d_x;            // d_x : [-]
     const VField<float> d_segmentLength;
     CurrentSliceInfo    d_csi;

     void setCurrentSliceInfo(CurrentSliceInfo const& csi) {
      d_csi = csi;
     }
     bool oneInputIsMV(size_t i) const {
       return (com::oneIsMV(d_prevFlowRate[i],d_prevInflow[i],d_inflow[i]) |
          com::oneIsMV(d_k[i], d_x[i],d_segmentLength[i]));
     }

     MGVisitor(
          Field & flowRate,
          const VField<INT4>&  nrTimeSlices,
          ExecArguments const& arg,
          LddGraph      const& lg,
          const Field&         timestepInSecsF
          ):

         TimeSliceVisitor(lg,nrTimeSlices,timestepInSecsF),

         MyResultMap(flowRate.dest_f()),

         IterationFlowRateMap(flowRate.nrValues()),
         IterationPrevFlowRateMap(flowRate.nrValues()),

         UpstreamSumIterationFlowRateMap(flowRate.nrValues()),
         UpstreamSumIterationPrevFlowRateMap(flowRate.nrValues()),

         d_prevFlowRate (arg[1], flowRate.nrValues()),
         d_prevInflow   (arg[2], flowRate.nrValues()),
         d_inflow       (arg[3], flowRate.nrValues()), // m3/sec
         d_k            (arg[4], flowRate.nrValues()),
         d_x            (arg[5], flowRate.nrValues()),
         d_segmentLength(arg[6], flowRate.nrValues())
       {
         for(size_t i=0; i <  flowRate.nrValues(); ++i) {
          if (oneInputIsMV(i)) {
                pcr::setMV(MyResultMap[i]);
           //     pcr::setMV(IterationFlowRateMap[i]);
           //     pcr::setMV(IterationPrevFlowRateMap[i]);
          } else {
             MyResultMap[i]= 0;
          }
        }
       }

  };

  ExecArguments arg(op,rte,nrArgs);
  ScopedLddGraph lgs(rte,arg[0]);
  LddGraph const& lg(lgs.current());

  arg.createResult();
  Field& flowRate(arg.result(0));

  const VField<INT4> nrTimeSlices(arg[7],flowRate.nrValues());

  PRECOND(arg.size() == 9);
  MGVisitor v(flowRate, nrTimeSlices, arg, lg, arg[8]);

  v.visitPerCachmentSlice();

  arg.pushResults();
}
#undef MG_DEBUG
