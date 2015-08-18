
# create html or DocBook table from
#  output of calc::OperationTimer::print()

 function rowDocBook(str) {
   gsub("td>","entry>",str);
   gsub("tr>","row>",str);
   return str;
 }

 BEGIN {html=0;
        head[1]="operation";
        head[2]="total time spent (%)";
        head[3]="cost p/operation";
        head[4]="nr of calls in dynamic";
        if(html) {
         print "<html><table rules='all' border='1'>";
         print "<tr>";
         printf " <td>%s</td><td>%s</td><td>%s</td><td>%s</td>\n",head[1],head[2],head[3],head[4];
         print "</tr>";
        } else {
          # docbook
          print "<table frame='all'><title>Measurements</title>";
          print "<tgroup cols='4'><thead><row>";
          printf " <entry>%s</entry><entry>%s</entry><entry>%s</entry><entry>%s</entry>\n", \
               head[1],head[2],head[3],head[4];
          print  "</row></thead>";
          print  "<tbody>";
        }
         nrLoops = 700.0;
         nrOps=0;
         totPerc=0;
       }
       {
         t= ($1 / 1024) * 1.0;

         name[NR]      = $3;
         totOpCost[NR] = t;
         totCost  += t;
         nrOpsInDynamic[NR] = $2/nrLoops;
         if ( $3 == "+" )
           stdCost = t/nrOpsInDynamic[NR];

         totOps+=nrOpsInDynamic[NR];

       }
 END   {
         onePerTot = totCost*0.01;
         rowStr="<tr><td>%s</td><td>%4.1f</td><td>%4.1f</td><td>%d</td></tr>\n";
         if (!html) {
          rowStr=rowDocBook(rowStr)
         }
         for(i=1; i <= NR; i++) {
          p = totOpCost[i]/onePerTot;
          totPerc+=p;
          printf rowStr, name[i],p, \
                 totOpCost[i]/nrOpsInDynamic[i]/stdCost, nrOpsInDynamic[i];
         }
         endStr="<tr><td>Total</td><td>%5.2f</td><td>-</td><td>%d</td></tr>\n";
         if (html) {
           printf endStr, totPerc,totOps;
           print "</table></html>";
         } else {
           endStr=rowDocBook(endStr);
           printf endStr, totPerc,totOps;
           print "</tbody></tgroup></table>";
         }
       }
