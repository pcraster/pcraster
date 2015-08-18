
// vanaf hier laten staan
More bindingRHS:
        | << std::vector<calc::ParsIndex *> indDef; >>
           TOK_LB
           << // par = [ index1, index2 ]
              testNoArray($par);
           >>
           indecesDef[indDef]
           TOK_RB
          << // array definition
             d_prolog->addSymbol(new ArrayDefinition(par,indDef));
          >>
        |<< GenId qId; >>
         indF:TOK_INDEX_F
         TOK_LP qid>[qId]
              { ~TOK_RP << expectFile("index-table"); >> }
         TOK_RP
         <<
           calc::BindedSymbol indexTableFile(qId);
           indexTableFile.setInputFilePath();
           if (!par.isArray()) {
            // pcrcalc/test275
            GenId f = genId(indF);
            f.posError("Function "+f.qName()+" only allowed on array parameters");
           }
           calc::IndexTable *p =
              dynamic_cast<calc::IndexTable *>
                     (par.block()->findSymbol(&indexTableFile,VS_INDEXTABLE,false));
           bool firstUse = !p;
           if (firstUse) {
             try {
               expectedFileType(indexTableFile.externalName(),VS_INDEXTABLE);
              } catch (com::Exception& msg) {
               indexTableFile.posError(msg.messages());
              }
              p = new calc::IndexTable(indexTableFile,par.descriptor());
           }
           // add these 2 symbols in this order; above func does a
           // more type specific (check in index type) test
           // (see test/pcrcalc277)
           try { d_prolog->addSymbol(
                       par.indexTable(p,true,tokenOp(indF)));
           } catch (com::Exception& msg) {
             indexTableFile.posError(msg.messages());
           }
           if (firstUse) // pcrcalc/test280b
             d_prolog->addSymbol(p);
         >>


indecesDef[std::vector<calc::ParsIndex* >& indDef]
        : << calc::ParsIndex *p;>>
                  indexDef>[p]
            << indDef.push_back(p); >>
            ( TOK_COMMA indexDef>[p]
              << indDef.push_back(p); >>
            )*
        ;

indexDef>[calc::ParsIndex *index ]
        : << bool on=true; >>
          {
            TOK_PLUS
            |
            TOK_MINUS << on=false; >>
          }
          name:TOK_ID
          (
           (
            TOK_IS (
                 qid > [GenId extName]
              <<
                 $index = new calc::ParsIndexName(on,genId(name),extName);
              >>
                |
             idList>[ calc::IdList set_list ]
              << $index = new calc::ParsIndexSet(on,genId(name),set_list); >>
            )
           )
           | << $index = new calc::ParsIndexName(on,genId(name)); >>
          )
         ;
