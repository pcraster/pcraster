/*
 * A n t l r  T r a n s l a t i o n  H e a d e r
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-1998
 * Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR19
 *
 *   antlr_1x -CC -w2 -gl -ck 2 -gx calcparser.g
 *
 */

#define ANTLR_VERSION	13319
#include "pcctscfg.h"
#include "pccts_stdio.h"
#line 1 "calcparser.g"
#include "tokens.h"

#include "stddefx.h"
#ifdef BORLANDC
#pragma warn -8004
#endif
#include <math.h>
#include <string>
#include <vector>

/* TODO
* vragen/opmerkingen na bestudering ANTLR 2.7.x docs:
*
* gebruik huidige flex  code
*  -> /tmp/antlr-2.7.2/examples/cpp/flexLexer
*  - schrijf parser om:
* laat AST dumpen, kijk naar AST viewer.
*  aandachts punten:
*  gebruik bestande Nodes?
*  /tmp/antlr-2.7.2/examples/cpp/heteroAST
* - kan alles met AST transformaties
* - hoe werken excepties?
* heeft aangemaakte C++ code de throws clause?
*
*/

// see known problems M19:
#define PURIFY(r,s)

// all that is in return stat's
// to Parser.h compiling
namespace calc {
class Constant;
struct ParsReportMoment;
class ParsIndex;
class ParsPar;
class DefPar;
class UsePar;
struct ConstructPar;
class  StatementBlock;
class  WriteInfo;
class  IdList;
class  FieldExpr;
class  Operator;
class  TimerValue;
class  Script;
class  RunSettings;
}

#ifndef INCLUDED_CALC_FIELDEXPRARGS
#include "calc_fieldexprargs.h"
#define INCLUDED_CALC_FIELDEXPRARGS
#endif
#ifndef INCLUDED_CALC_SYMBOL
#include "calc_symbol.h"
#define INCLUDED_CALC_SYMBOL
#endif

#include "ATokPtr.h"
#define EXCEPTION_HANDLING
#define NUM_SIGNALS 3
#include "AParser.h"
#include "Parser.h"
#include "ATokPtr.h"
#ifndef PURIFY
#define PURIFY(r,s) memset((char *) &(r),'\0',(s));
#endif
#line 61 "calcparser.g"


#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif
#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#include "calc_arraydefvector.h"
#define INCLUDED_CALC_ARRAYDEFVECTOR
#endif
#ifndef INCLUDED_CALC_ASSIGNMENT
#include "calc_assignment.h"
#define INCLUDED_CALC_ASSIGNMENT
#endif
#ifndef INCLUDED_CALC_BRANCHEXPRIMPL
#include "calc_branchexprimpl.h"
#define INCLUDED_CALC_BRANCHEXPRIMPL
#endif
#ifndef INCLUDED_CALC_CONSTANT
#include "calc_constant.h"
#define INCLUDED_CALC_CONSTANT
#endif
#ifndef INCLUDED_CALC_CONSTRUCTPAR
#include "calc_constructpar.h"
#define INCLUDED_CALC_CONSTRUCTPAR
#endif
#ifndef INCLUDED_CALC_DEFPAR
#include "calc_defpar.h"
#define INCLUDED_CALC_DEFPAR
#endif
#ifndef INCLUDED_CALC_DOUBLEASS
#include "calc_doubleass.h"
#define INCLUDED_CALC_DOUBLEASS
#endif
#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
#endif
#ifndef INCLUDED_CALC_FIELDLEAF
#include "calc_fieldleaf.h"
#define INCLUDED_CALC_FIELDLEAF
#endif
#ifndef INCLUDED_CALC_FIELDNRPARAMETER
#include "calc_fieldnrparameter.h"
#define INCLUDED_CALC_FIELDNRPARAMETER
#endif
#ifndef  INCLUDED_CALC_INDEXTABLE
#include "calc_indextable.h"
#define  INCLUDED_CALC_INDEXTABLE
#endif
#ifndef INCLUDED_CALC_FOREACH
#include "calc_foreach.h"
#define INCLUDED_CALC_FOREACH
#endif
#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif
#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif
#ifndef INCLUDED_CALC_INFOSCRIPT
#include "calc_infoscript.h"
#define INCLUDED_CALC_INFOSCRIPT
#endif
#ifndef INCLUDED_CALC_LOOKUPEXPR
#include "calc_lookupexpr.h"
#define INCLUDED_CALC_LOOKUPEXPR
#endif
#ifndef INCLUDED_CALC_MODELLINKINIT
#include "calc_modellinkinit.h"
#define INCLUDED_CALC_MODELLINKINIT
#endif
#ifndef INCLUDED_CALC_MODELLINKMETHODEXPR
#include "calc_modellinkmethodexpr.h"
#define INCLUDED_CALC_MODELLINKMETHODEXPR
#endif
#ifndef INCLUDED_CALC_MODELLINKMETHODSTATEMENT
#include "calc_modellinkmethodstatement.h"
#define INCLUDED_CALC_MODELLINKMETHODSTATEMENT
#endif

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_PARSINDEX
#include "calc_parsindex.h"
#define INCLUDED_CALC_PARSINDEX
#endif
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif
#ifndef INCLUDED_CALC_SCRIPT
#include "calc_script.h"
#define INCLUDED_CALC_SCRIPT
#endif
#ifndef INCLUDED_CALC_STACKINPUT
#include "calc_stackinput.h"
#define INCLUDED_CALC_STACKINPUT
#endif
#ifndef INCLUDED_CALC_STDOUTSTAT
#include "calc_stdoutstat.h"
#define INCLUDED_CALC_STDOUTSTAT
#endif
#ifndef INCLUDED_CALC_TIMEINPUTEXPR
#include "calc_timeinputexpr.h"
#define INCLUDED_CALC_TIMEINPUTEXPR
#endif
#ifndef INCLUDED_CALC_TIMEOUTPUT
#include "calc_timeoutput.h"
#define INCLUDED_CALC_TIMEOUTPUT
#endif
#ifndef INCLUDED_CALC_TIMERVALUE
#include "calc_timervalue.h"
#define INCLUDED_CALC_TIMERVALUE
#endif
#ifndef INCLUDED_CALC_USEPAR
#include "calc_usepar.h"
#define INCLUDED_CALC_USEPAR
#endif
#ifndef INCLUDED_CALC_WRITEINFO
#include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif

#ifndef INCLUDED_LEXGRAMMAR
#include "lexgrammar.h"
#define INCLUDED_LEXGRAMMAR
#endif

typedef calc::LexToken ANTLRToken;

#define NEW_EXPR(opTokenPtr,op,args) \
( new calc::BranchExprImpl(element(opTokenPtr),(op),(args)))

calc::Script *Parser::script() const
{
	return d_script;
};

calc::Element Parser::element(const ANTLRTokenPtr at) const
{
	return calc::Element(script(),mytoken(at)->position());
};

calc::Symbol Parser::symbol(const ANTLRTokenPtr at) const
{
	return calc::Symbol(script(), mytoken(at));
};

const calc::Operator& Parser::tokenOp(const ANTLRTokenPtr at) const
{
	return calc::major2op(mytoken(at)->opCode());
}

//! current token saus this is an exp where we expect a file
void Parser::expectFile(const std::string& fileDescr) {
	symbol(LT(1)).posError("Expression illegal here, expecting name of "+fileDescr);
}

void Parser::checkParseError() {
	const ANTLRTokenPtr at(LT(1));
	calc::Symbol p=symbol(at);
	// pcrcalc/test34
	std::ostringstream msg;
	bool kw=mytoken(at)->isKeyword();
	msg  << "Syntax error at " << (kw ? "keyword ":"symbol ") << p.qName();
	if (kw)
	msg  << "\n Keywords can not be used as names for files or variables"
	<< "\n Keywords must be placed in a specific order";
	p.posError(msg.str());
}

void Parser::errorFuncInDynamic(const calc::Symbol& s) const {
	s.posError("function "+s.qName()+
	" is only legal in the dynamic section");
};

void Parser::illegalFunc(const calc::Symbol& s) const {
// pcrcalc/test41
s.posError("function "+s.qName()+" illegal here");
}

void Parser::notAFunc(const calc::Symbol& s) const {
s.posError(s.qName()+" is not a function");
}

calc::Constant Parser::createCastedConstant(
const ANTLRTokenPtr   convF,
const calc::Symbol& nr) const
{
calc::Symbol s(symbol(convF));
std::ostringstream name;
name << s.name() << "(" << nr.name() << ")";
s.setName(name.str());
return calc::Constant(s, tokenOp(convF).vs(),nr);
}

void Parser::
zzdflthandlers( int _signal, int *_retsignal )
{
	*_retsignal = _signal;
}


void
Parser::externalBindings(int *_retsignal, calc::RunSettings& rs )
{
#line 385 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr left=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 396 "calcparser.g"
	{
		while ( (LA(1)==TOK_ID) ) {
#line 387 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			left = (ANTLRTokenPtr)LT(1);
 consume();
#line 387 "calcparser.g"
			zzmatch_wsig(TOK_IS, _handler);
			
#line 388 "calcparser.g"
			calc::Symbol s;
 consume();
#line 389 "calcparser.g"
			{
				if ( (setwd1[LA(1)]&0x1) ) {
#line 389 "calcparser.g"
					 s  = number(&_signal); if (_signal) goto _handler;

				}
				else {
					if ( (setwd1[LA(1)]&0x2) ) {
#line 390 "calcparser.g"
						 s  = qid(&_signal); if (_signal) goto _handler;

					}
					else {
						if (_sva) _signal=NoViableAlt;
						else _signal=NoSemViableAlt;
						goto _handler;  /* MR7 */
					}
				}
			}
#line 392 "calcparser.g"
			
			rs.add(symbol(left), s);
#line 395 "calcparser.g"
			{
				if ( (LA(1)==TOK_SC) ) {
#line 395 "calcparser.g"
					zzmatch_wsig(TOK_SC, _handler);
					 consume();
				}
			}
		}
	}
#line 396 "calcparser.g"
	zzmatch_wsig(TOK_EOF, _handler);
	 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd1, 0x4);
	return;
	/* exception handlers */
_handler:
	switch ( _signal ) {
	case NoSignal: break;  /* MR7 */
	default :
		
		checkParseError();
		_signal=NoSignal;  /* MR7 */
		break;  /* MR7 */
	}
_adios:
	return;
}

void
Parser::model(int *_retsignal)
{
#line 404 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
	if ( (setwd1[LA(1)]&0x8) && 
(setwd1[LA(2)]&0x10) ) {
#line 404 "calcparser.g"
		{
			if ( (LA(1)==TOK_MODEL) ) {
#line 404 "calcparser.g"
				zzmatch_wsig(TOK_MODEL, _handler);
				 consume();
			}
		}
#line 405 "calcparser.g"
		codeBlocks(&_signal, script() ); if (_signal) goto _handler;
#line 405 "calcparser.g"
		zzmatch_wsig(TOK_EOF, _handler);
		 consume();
	}
	else {
		if ( (setwd1[LA(1)]&0x20) && (setwd1[LA(2)]&0x40) ) {
#line 406 "calcparser.g"
			modelProlog(&_signal); if (_signal) goto _handler;
#line 406 "calcparser.g"
			modelCode(&_signal); if (_signal) goto _handler;
#line 406 "calcparser.g"
			zzmatch_wsig(TOK_EOF, _handler);
			 consume();
		}
		else {
			if (_sva) _signal=NoViableAlt;
			else _signal=NoSemViableAlt;
			goto _handler;  /* MR7 */
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd1, 0x80);
	return;
	/* exception handlers */
_handler:
	switch ( _signal ) {
	case NoSignal: break;  /* MR7 */
	default :
		
		checkParseError();
		_signal=NoSignal;  /* MR7 */
		break;  /* MR7 */
	}
_adios:
	return;
}

void
Parser::modelProlog(int *_retsignal)
{
#line 414 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 414 "calcparser.g"
	{
		if ( (LA(1)==TOK_BINDING) ) {
#line 414 "calcparser.g"
			zzmatch_wsig(TOK_BINDING, _handler);
			 consume();
#line 414 "calcparser.g"
			bindingSection(&_signal); if (_signal) goto _handler;
		}
	}
#line 414 "calcparser.g"
	{
		if ( (LA(1)==TOK_AREAMAP) ) {
#line 414 "calcparser.g"
			areaMapSection(&_signal); if (_signal) goto _handler;
		}
	}
#line 414 "calcparser.g"
	{
		if ( (LA(1)==TOK_TIMER)
 ) {
#line 414 "calcparser.g"
			timer(&_signal); if (_signal) goto _handler;
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x1);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::modelCode(int *_retsignal)
{
#line 416 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr pos=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 416 "calcparser.g"
	{
		if ( (setwd2[LA(1)]&0x2) ) {
#line 416 "calcparser.g"
			{
				if ( (LA(1)==TOK_INITIAL) ) {
#line 416 "calcparser.g"
					zzmatch_wsig(TOK_INITIAL, _handler);
					 consume();
				}
				else {
					if ( (LA(1)==TOK_MODEL) ) {
#line 416 "calcparser.g"
						zzmatch_wsig(TOK_MODEL, _handler);
						 consume();
					}
					else {
						if (_sva) _signal=NoViableAlt;
						else _signal=NoSemViableAlt;
						goto _handler;  /* MR7 */
					}
				}
			}
#line 416 "calcparser.g"
			codeBlocks(&_signal, script() ); if (_signal) goto _handler;
		}
	}
#line 418 "calcparser.g"
	{
		if ( (LA(1)==TOK_DYNAMIC) ) {
#line 418 "calcparser.g"
			zzmatch_wsig(TOK_DYNAMIC, _handler);
			
			pos = (ANTLRTokenPtr)LT(1);

#line 419 "calcparser.g"
			
			calc::Element p(element(pos));
			if (!script()->isDynamicModel()) // pcrcalc/test8a
			p.posError("There is a dynamic section but no timer section");
			// create and add it to the script
			calc::DynamicSection *n= new calc::DynamicSection(p,script());
			script()->addStatement(n);
 consume();
#line 427 "calcparser.g"
			codeBlocks(&_signal, n ); if (_signal) goto _handler;
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x4);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::bindingSection(int *_retsignal)
{
#line 431 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 431 "calcparser.g"
	{
		while ( (setwd2[LA(1)]&0x8)
 ) {
#line 431 "calcparser.g"
			bindingDefinition(&_signal); if (_signal) goto _handler;
		}
	}
#line 432 "calcparser.g"
	script()->evaluateBindings();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x10);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::areaMapSection(int *_retsignal)
{
#line 436 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 436 "calcparser.g"
	calc::Symbol s;
#line 437 "calcparser.g"
	zzmatch_wsig(TOK_AREAMAP, _handler);
	 consume();
#line 437 "calcparser.g"
	 s  = qid(&_signal); if (_signal) goto _handler;

#line 437 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	
#line 438 "calcparser.g"
	
	// can be binded symbol test46
	script()->setAreaMap(s);
 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x20);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::timer(int *_retsignal)
{
#line 444 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr tssID=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 444 "calcparser.g"
	calc::TimerValue start,end,step;
#line 445 "calcparser.g"
	zzmatch_wsig(TOK_TIMER, _handler);
	 consume();
#line 454 "calcparser.g"
	{
		if ( (setwd2[LA(1)]&0x40) && (setwd2[LA(2)]&0x80) ) {
#line 447 "calcparser.g"
			{
#line 447 "calcparser.g"
				 start  = timerValue(&_signal); if (_signal) goto _handler;

#line 448 "calcparser.g"
				 end  = timerValue(&_signal); if (_signal) goto _handler;

#line 449 "calcparser.g"
				 step  = timerValue(&_signal); if (_signal) goto _handler;

#line 450 "calcparser.g"
				
				calc::checkTimerSection(start,end,step);
				script()->setTimer(start.value(),end.value(),step.value());
			}
		}
		else {
			if ( (LA(1)==TOK_ID) && (LA(2)==TOK_SC) ) {
#line 454 "calcparser.g"
				zzmatch_wsig(TOK_ID, _handler);
				
				tssID = (ANTLRTokenPtr)LT(1);

#line 455 "calcparser.g"
				
				script()->setTimer(symbol(tssID));
 consume();
			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
#line 458 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	 consume();
#line 459 "calcparser.g"
	{
		while ( (LA(1)==TOK_ID) ) {
#line 459 "calcparser.g"
			reportMomentDef(&_signal); if (_signal) goto _handler;
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x1);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 calc::TimerValue  
Parser::timerValue(int *_retsignal)
{
	 calc::TimerValue  	 _retv;
#line 462 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::TimerValue  	))
	*_retsignal = NoSignal;
#line 463 "calcparser.g"
	calc::Symbol sym;
#line 464 "calcparser.g"
	{
		if ( (setwd3[LA(1)]&0x2) ) {
#line 464 "calcparser.g"
			 sym  = number(&_signal); if (_signal) goto _handler;

		}
		else {
			if ( (setwd3[LA(1)]&0x4)
 ) {
#line 464 "calcparser.g"
				 sym  = qid(&_signal); if (_signal) goto _handler;

			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
#line 465 "calcparser.g"
	
	_retv = calc::TimerValue(sym);
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x8);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

void
Parser::reportMomentDef(int *_retsignal)
{
#line 470 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr reportId=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 470 "calcparser.g"
	std::vector<calc::ParsReportMoment> m; calc::Symbol s;
#line 471 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	reportId = (ANTLRTokenPtr)LT(1);

#line 471 "calcparser.g"
	s = symbol(reportId);
 consume();
#line 472 "calcparser.g"
	zzmatch_wsig(TOK_IS, _handler);
	 consume();
#line 472 "calcparser.g"
	reportMoments(&_signal, m ); if (_signal) goto _handler;
#line 472 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	
#line 473 "calcparser.g"
	script()->addReport(
	new calc::ReportDefinition(s,m,(int)script()->nrTimeSteps()));
 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x10);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::bindingDefinition(int *_retsignal)
{
#line 479 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 479 "calcparser.g"
	 calc::DefPar p  = parWithIndeces(&_signal, script() ); if (_signal) goto _handler;

#line 479 "calcparser.g"
	zzmatch_wsig(TOK_IS, _handler);
	 consume();
#line 479 "calcparser.g"
	bindingRight(&_signal, p ); if (_signal) goto _handler;
#line 479 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x20);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::bindingRight(int *_retsignal, const calc::DefPar& par  )
{
#line 482 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr convF=NULL, indF=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 483 "calcparser.g"
	calc::Symbol right;
	if ( (setwd3[LA(1)]&0x40) ) {
#line 484 "calcparser.g"
		 right  = qid(&_signal); if (_signal) goto _handler;

#line 485 "calcparser.g"
		
		if ( par.isArray())
		par.posError("Illegal construct");
		script()->addBinding( par,right);
#line 490 "calcparser.g"
		{
			if ( (LA(1)==TOK_LP) ) {
#line 490 "calcparser.g"
				zzmatch_wsig(TOK_LP, _handler);
				
#line 490 "calcparser.g"
				illegalFunc(right);
 consume();
			}
		}
	}
	else {
		if ( (setwd3[LA(1)]&0x80) ) {
#line 491 "calcparser.g"
			calc::Symbol nr;
#line 492 "calcparser.g"
			 nr  = number(&_signal); if (_signal) goto _handler;

#line 493 "calcparser.g"
			
			if ( par.isArray())
			par.posError("Illegal construct");
			script()->addBinding( par,nr);
		}
		else {
			if ( (LA(1)==TOK_CONV_F) ) {
#line 498 "calcparser.g"
				calc::Symbol nr;
#line 499 "calcparser.g"
				zzmatch_wsig(TOK_CONV_F, _handler);
				
				convF = (ANTLRTokenPtr)LT(1);
 consume();
#line 499 "calcparser.g"
				zzmatch_wsig(TOK_LP, _handler);
				 consume();
#line 499 "calcparser.g"
				 nr  = number(&_signal); if (_signal) goto _handler;

#line 500 "calcparser.g"
				
				if ( par.isArray())
				par.posError("Illegal construct");
#line 504 "calcparser.g"
				zzmatch_wsig(TOK_RP, _handler);
				
#line 505 "calcparser.g"
				// add numeric binding with a typecast
				(void)createCastedConstant(convF,nr); // pcrcalc/test42
				script()->addBinding( par,nr, tokenOp(convF).vs());
 consume();
			}
			else {
				if ( (LA(1)==TOK_LB)
 ) {
#line 509 "calcparser.g"
					std::vector<calc::ParsIndex *> indDef;
#line 510 "calcparser.g"
					zzmatch_wsig(TOK_LB, _handler);
					
#line 511 "calcparser.g"
					// par = [ index1, index2 ]
					if ( par.isArray())
					par.posError("Illegal construct");
 consume();
#line 515 "calcparser.g"
					indecesDef(&_signal, indDef ); if (_signal) goto _handler;
#line 516 "calcparser.g"
					zzmatch_wsig(TOK_RB, _handler);
					
#line 517 "calcparser.g"
					// array definition
					script()->addSymbol(new calc::ArrayDefinition(par,indDef));
 consume();
				}
				else {
					if ( (LA(1)==TOK_INDEX_F) ) {
#line 520 "calcparser.g"
						calc::Symbol qId;
#line 521 "calcparser.g"
						zzmatch_wsig(TOK_INDEX_F, _handler);
						
						indF = (ANTLRTokenPtr)LT(1);
 consume();
#line 522 "calcparser.g"
						zzmatch_wsig(TOK_LP, _handler);
						 consume();
#line 522 "calcparser.g"
						 qId  = qid(&_signal); if (_signal) goto _handler;

#line 523 "calcparser.g"
						{
							if ( (setwd4[LA(1)]&0x1) ) {
#line 523 "calcparser.g"
								zzsetmatch_wsig(err1, _handler);
								
#line 523 "calcparser.g"
								expectFile("index-table");
 consume();
							}
						}
#line 524 "calcparser.g"
						zzmatch_wsig(TOK_RP, _handler);
						
#line 525 "calcparser.g"
						
						calc::BindedSymbol indexTableFile(qId);
						indexTableFile.setInputFilePath();
						if (!par.isArray()) {
							// pcrcalc/test275
							calc::Symbol f = symbol(indF);
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
						try { script()->addSymbol(
							par.indexTable(p,true,tokenOp(indF)));
						} catch (com::Exception& msg) {
							indexTableFile.posError(msg.messages());
						}
						if (firstUse) // pcrcalc/test280b
						script()->addSymbol(p);
 consume();
					}
					else {
						if (_sva) _signal=NoViableAlt;
						else _signal=NoSemViableAlt;
						goto _handler;  /* MR7 */
					}
				}
			}
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd4, 0x2);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::indecesDef(int *_retsignal, std::vector<calc::ParsIndex* >& indDef )
{
#line 558 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 559 "calcparser.g"
	calc::ParsIndex *p;
#line 560 "calcparser.g"
	 p  = indexDef(&_signal); if (_signal) goto _handler;

#line 561 "calcparser.g"
	indDef.push_back(p);
#line 564 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA) ) {
#line 562 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 562 "calcparser.g"
			 p  = indexDef(&_signal); if (_signal) goto _handler;

#line 563 "calcparser.g"
			indDef.push_back(p);
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd4, 0x4);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 calc::ParsIndex *  
Parser::indexDef(int *_retsignal)
{
	 calc::ParsIndex *  	 _retv;
#line 567 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr name=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::ParsIndex *  	))
	*_retsignal = NoSignal;
#line 568 "calcparser.g"
	bool on=true;
#line 570 "calcparser.g"
	{
		if ( (LA(1)==TOK_PLUS) ) {
#line 570 "calcparser.g"
			zzmatch_wsig(TOK_PLUS, _handler);
			 consume();
		}
		else {
			if ( (LA(1)==TOK_MINUS)
 ) {
#line 572 "calcparser.g"
				zzmatch_wsig(TOK_MINUS, _handler);
				
#line 572 "calcparser.g"
				on=false;
 consume();
			}
		}
	}
#line 574 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	name = (ANTLRTokenPtr)LT(1);
 consume();
#line 587 "calcparser.g"
	{
		if ( (LA(1)==TOK_IS) ) {
#line 577 "calcparser.g"
			{
#line 577 "calcparser.g"
				zzmatch_wsig(TOK_IS, _handler);
				 consume();
#line 578 "calcparser.g"
				{
					if ( (setwd4[LA(1)]&0x8) ) {
#line 578 "calcparser.g"
						 calc::Symbol extName  = qid(&_signal); if (_signal) goto _handler;

#line 579 "calcparser.g"
						
						_retv = new calc::ParsIndexName(on,symbol(name),extName);
					}
					else {
						if ( (LA(1)==TOK_LP) ) {
#line 583 "calcparser.g"
							  calc::IdList set_list   = idList(&_signal); if (_signal) goto _handler;

#line 584 "calcparser.g"
							_retv = new calc::ParsIndexSet(on,symbol(name),set_list);
						}
						else {
							if (_sva) _signal=NoViableAlt;
							else _signal=NoSemViableAlt;
							goto _handler;  /* MR7 */
						}
					}
				}
			}
		}
		else {
			if ( (setwd4[LA(1)]&0x10) ) {
#line 587 "calcparser.g"
				_retv = new calc::ParsIndexName(on,symbol(name));
			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd4, 0x20);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

void
Parser::reportMoments(int *_retsignal, std::vector<calc::ParsReportMoment> &m )
{
#line 591 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 592 "calcparser.g"
	calc::ParsReportMoment p;
#line 593 "calcparser.g"
	reportMoment(&_signal, p ); if (_signal) goto _handler;
#line 594 "calcparser.g"
	m.push_back(p);
#line 597 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA)
 ) {
#line 595 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 595 "calcparser.g"
			reportMoment(&_signal, p ); if (_signal) goto _handler;
#line 596 "calcparser.g"
			m.push_back(p);
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd4, 0x40);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::reportMoment(int *_retsignal, calc::ParsReportMoment& p )
{
#line 600 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr start=NULL, range=NULL, range2=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 601 "calcparser.g"
	p.start=0;  p.step=0; p.end=0;
#line 617 "calcparser.g"
	{
		if ( (LA(1)==TOK_INT) ) {
#line 603 "calcparser.g"
			{
#line 603 "calcparser.g"
				zzmatch_wsig(TOK_INT, _handler);
				
				start = (ANTLRTokenPtr)LT(1);

#line 603 "calcparser.g"
				p.start = mytoken(start)->integerVal();
 consume();
#line 606 "calcparser.g"
				{
					if ( (LA(1)==TOK_PLUS) ) {
#line 604 "calcparser.g"
						{
#line 604 "calcparser.g"
							  p.step  = reportMomentStep(&_signal); if (_signal) goto _handler;

#line 604 "calcparser.g"
							zzmatch_wsig(TOK_MOMENT_RANGE, _handler);
							
							range = (ANTLRTokenPtr)LT(1);

#line 605 "calcparser.g"
							p.end = mytoken(range)->integerVal();
 consume();
						}
					}
					else {
						if ( (setwd4[LA(1)]&0x80) ) {
#line 607 "calcparser.g"
							{
								if ( (LA(1)==TOK_MOMENT_RANGE) ) {
#line 607 "calcparser.g"
									zzmatch_wsig(TOK_MOMENT_RANGE, _handler);
									
									range2 = (ANTLRTokenPtr)LT(1);

#line 608 "calcparser.g"
									p.end = mytoken(range2)->integerVal();
 consume();
								}
							}
						}
						else {
							if (_sva) _signal=NoViableAlt;
							else _signal=NoSemViableAlt;
							goto _handler;  /* MR7 */
						}
					}
				}
#line 611 "calcparser.g"
				try {  p.check();
				} catch (com::Exception& msg) {
					element(start).posError(msg.messages());
				}
			}
		}
		else {
			if ( (LA(1)==TOK_ENDTIME)
 ) {
#line 617 "calcparser.g"
				zzmatch_wsig(TOK_ENDTIME, _handler);
				
#line 617 "calcparser.g"
				p.start= -1;
 consume();
			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x1);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 int  
Parser::reportMomentStep(int *_retsignal)
{
	 int  	 _retv;
#line 621 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr v=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( int  	))
	*_retsignal = NoSignal;
#line 622 "calcparser.g"
	zzmatch_wsig(TOK_PLUS, _handler);
	 consume();
#line 622 "calcparser.g"
	zzmatch_wsig(TOK_INT, _handler);
	
	v = (ANTLRTokenPtr)LT(1);

#line 623 "calcparser.g"
	_retv = mytoken(v)->integerVal();
 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x2);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

  calc::IdList   
Parser::idList(int *_retsignal)
{
	  calc::IdList   	 _retv;
#line 631 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr firstId=NULL, nextId=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof(  calc::IdList   	))
	*_retsignal = NoSignal;
#line 632 "calcparser.g"
	std::vector<calc::Symbol> list; calc::Symbol s;
#line 633 "calcparser.g"
	zzmatch_wsig(TOK_LP, _handler);
	 consume();
#line 634 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	firstId = (ANTLRTokenPtr)LT(1);

#line 634 "calcparser.g"
	list.push_back(symbol(firstId));
 consume();
#line 636 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA) ) {
#line 635 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 635 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			nextId = (ANTLRTokenPtr)LT(1);

#line 635 "calcparser.g"
			list.push_back(symbol(nextId));
 consume();
		}
	}
#line 637 "calcparser.g"
	zzmatch_wsig(TOK_RP, _handler);
	
#line 638 "calcparser.g"
	_retv = calc::IdList(list);
 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x4);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

void
Parser::codeBlocks(int *_retsignal, calc::StatementBlock *inclIn )
{
#line 641 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 652 "calcparser.g"
	{
		while ( 1 ) {
			if ( !((setwd5[LA(1)]&0x8))) break;
			if ( (LA(1)==TOK_FOREACH) ) {
#line 643 "calcparser.g"
				foreach(&_signal,  inclIn ); if (_signal) goto _handler;
			}
			else {
				if ( (LA(1)==TOK_REPEAT) ) {
#line 644 "calcparser.g"
					repeat(&_signal,  inclIn ); if (_signal) goto _handler;
				}
				else {
					if ( (setwd5[LA(1)]&0x10)
 ) {
#line 649 "calcparser.g"
						{
#line 646 "calcparser.g"
							calc::InnerStatementBlock *b =
							new calc::InnerStatementBlock(element(LT(1)), inclIn);
							inclIn->addStatement(b);
#line 650 "calcparser.g"
							codeBlock(&_signal, b ); if (_signal) goto _handler;
#line 650 "calcparser.g"
							{
								while ( (setwd5[LA(1)]&0x20) && (setwd5[LA(2)]&0x40) ) {
#line 650 "calcparser.g"
									codeBlock(&_signal, b ); if (_signal) goto _handler;
								}
							}
						}
					}
					else break; /* MR6 code for exiting loop "for sure" */
				}
			}
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x80);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::codeBlock(int *_retsignal, calc::StatementBlock *inclIn )
{
#line 655 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr v=NULL, modelId=NULL, modelName=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 656 "calcparser.g"
	bool hasReport=false;
	const calc::Report *r=0;
	static int  inSituNr=0; // harmless static
	if ( (setwd6[LA(1)]&0x1) ) {
#line 660 "calcparser.g"
		{
			if ( (LA(1)==TOK_REPORT) ) {
#line 660 "calcparser.g"
				zzmatch_wsig(TOK_REPORT, _handler);
				
#line 661 "calcparser.g"
				
				hasReport=true;
				script()->setReportFound();
				std::vector<calc::ParsReportMoment> inSitu;
 consume();
#line 667 "calcparser.g"
				{
					if ( (LA(1)==TOK_LP) ) {
#line 667 "calcparser.g"
						zzmatch_wsig(TOK_LP, _handler);
						 consume();
#line 669 "calcparser.g"
						{
							if ( (LA(1)==TOK_ID)
 ) {
#line 669 "calcparser.g"
								zzmatch_wsig(TOK_ID, _handler);
								
								v = (ANTLRTokenPtr)LT(1);

#line 669 "calcparser.g"
								r = script()->findReport(symbol(v));
 consume();
							}
							else {
								if ( (setwd6[LA(1)]&0x2) ) {
#line 670 "calcparser.g"
									reportMoments(&_signal, inSitu ); if (_signal) goto _handler;
#line 671 "calcparser.g"
									
									char buf[8];
									sprintf(buf,"%d",inSituNr++);
									calc::Symbol s(script(),buf,0);
									calc::ReportDefinition *rd;
									rd = new calc::ReportDefinition(
									script()->generatedSymbol("reportMoments",buf),
									inSitu,(int)script()->nrTimeSteps());
									r=rd;
									script()->addReport(rd);
								}
								else {
									if (_sva) _signal=NoViableAlt;
									else _signal=NoSemViableAlt;
									goto _handler;  /* MR7 */
								}
							}
						}
#line 683 "calcparser.g"
						zzmatch_wsig(TOK_RP, _handler);
						 consume();
					}
				}
			}
		}
#line 685 "calcparser.g"
		assignment(&_signal,  inclIn,calc::WriteInfo(script(),hasReport,r,inclIn->inDynamic()) ); if (_signal) goto _handler;
	}
	else {
		if ( (LA(1)==TOK_FILEOUTPUT) ) {
#line 686 "calcparser.g"
			zzmatch_wsig(TOK_FILEOUTPUT, _handler);
			 consume();
#line 686 "calcparser.g"
			 calc::FieldExpr *r_expr  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 686 "calcparser.g"
			statementSep(&_signal); if (_signal) goto _handler;
#line 687 "calcparser.g"
			
			inclIn->addStatement(new calc::StdoutStatement(r_expr));
		}
		else {
			if ( (LA(1)==TOK_MODELLINK) ) {
#line 690 "calcparser.g"
				calc::FieldExprArgs args;
				std::string strArg;
#line 693 "calcparser.g"
				zzmatch_wsig(TOK_MODELLINK, _handler);
				 consume();
#line 693 "calcparser.g"
				zzmatch_wsig(TOK_ID, _handler);
				
				modelId = (ANTLRTokenPtr)LT(1);
 consume();
#line 693 "calcparser.g"
				zzmatch_wsig(TOK_IS, _handler);
				 consume();
#line 693 "calcparser.g"
				zzmatch_wsig(TOK_ID, _handler);
				
				modelName = (ANTLRTokenPtr)LT(1);
 consume();
#line 693 "calcparser.g"
				modelLinkArgs(&_signal,  inclIn,strArg,args ); if (_signal) goto _handler;
#line 693 "calcparser.g"
				statementSep(&_signal); if (_signal) goto _handler;
#line 694 "calcparser.g"
				inclIn->addStatement(new
				calc::ModelLinkInit(symbol(modelId),symbol(modelName),strArg,args));
			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x4);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::foreach(int *_retsignal, calc::StatementBlock *inclIn )
{
#line 699 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr posF=NULL, iterId=NULL, inId=NULL, exceptId=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 700 "calcparser.g"
	calc::ForEach *f=0;
	calc::Symbol iter;
	calc::IdList in,except,s; bool ascending=true;
#line 703 "calcparser.g"
	zzmatch_wsig(TOK_FOREACH, _handler);
	
	posF = (ANTLRTokenPtr)LT(1);
 consume();
#line 704 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	iterId = (ANTLRTokenPtr)LT(1);
 consume();
#line 705 "calcparser.g"
	zzmatch_wsig(TOK_IN, _handler);
	 consume();
#line 705 "calcparser.g"
	{
		if ( (LA(1)==TOK_ID) ) {
#line 705 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			inId = (ANTLRTokenPtr)LT(1);

#line 705 "calcparser.g"
			in = symbol(inId);
 consume();
		}
		else {
			if ( (LA(1)==TOK_LP)
 ) {
#line 706 "calcparser.g"
				 in  = idList(&_signal); if (_signal) goto _handler;

			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
#line 708 "calcparser.g"
	{
		if ( (LA(1)==TOK_EXCEPT) ) {
#line 708 "calcparser.g"
			zzmatch_wsig(TOK_EXCEPT, _handler);
			 consume();
#line 709 "calcparser.g"
			{
				if ( (LA(1)==TOK_ID) ) {
#line 709 "calcparser.g"
					zzmatch_wsig(TOK_ID, _handler);
					
					exceptId = (ANTLRTokenPtr)LT(1);

#line 709 "calcparser.g"
					except = symbol(exceptId);
 consume();
				}
				else {
					if ( (LA(1)==TOK_LP) ) {
#line 710 "calcparser.g"
						 except  = idList(&_signal); if (_signal) goto _handler;

					}
					else {
						if (_sva) _signal=NoViableAlt;
						else _signal=NoSemViableAlt;
						goto _handler;  /* MR7 */
					}
				}
			}
		}
	}
#line 713 "calcparser.g"
	zzmatch_wsig(TOK_LC, _handler);
	
#line 714 "calcparser.g"
	f =
	new calc::ForEach(element(posF),inclIn,
	symbol(iterId),in,except,s,ascending);
	inclIn->addStatement(f);
 consume();
#line 719 "calcparser.g"
	codeBlocks(&_signal, f ); if (_signal) goto _handler;
#line 720 "calcparser.g"
	zzmatch_wsig(TOK_RC, _handler);
	 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x8);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::repeat(int *_retsignal, calc::StatementBlock *inclIn )
{
#line 723 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr posR=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 724 "calcparser.g"
	calc::RepeatUntil *ru=0;
	calc::FieldExpr   *condition;
#line 727 "calcparser.g"
	zzmatch_wsig(TOK_REPEAT, _handler);
	
	posR = (ANTLRTokenPtr)LT(1);
 consume();
#line 728 "calcparser.g"
	zzmatch_wsig(TOK_LC, _handler);
	
#line 729 "calcparser.g"
	ru = new calc::RepeatUntil(element(posR),inclIn);
	inclIn->addStatement(ru);
 consume();
#line 732 "calcparser.g"
	codeBlocks(&_signal, ru ); if (_signal) goto _handler;
#line 733 "calcparser.g"
	zzmatch_wsig(TOK_RC, _handler);
	 consume();
#line 733 "calcparser.g"
	zzmatch_wsig(TOK_UNTIL, _handler);
	 consume();
#line 733 "calcparser.g"
	 condition  = expr(&_signal, ru ); if (_signal) goto _handler;

#line 733 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	
#line 734 "calcparser.g"
	{
		calc::FieldExprArgs args(1);
		args[0] = condition;
		ru->addCondition(
		new calc::BranchExprImpl(
		*(args[0]),calc::major2op(OP_TEST_UNTIL),args));
	}
 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x10);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::assignment(int *_retsignal, calc::StatementBlock *inclIn,const calc::WriteInfo& w )
{
#line 747 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 749 "calcparser.g"
	 calc::UsePar par  = parWithIndeces(&_signal, inclIn ); if (_signal) goto _handler;

#line 750 "calcparser.g"
	assignmentTail(&_signal,  inclIn,w,par ); if (_signal) goto _handler;
#line 751 "calcparser.g"
	statementSep(&_signal); if (_signal) goto _handler;
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x20);
	return;
	/* exception handlers */
_handler:
	switch ( _signal ) {
	case NoSignal: break;  /* MR7 */
	default :
		/* see pcrcalc/test34 without this no cleaning? */;
		checkParseError();
		_signal=NoSignal;  /* MR7 */
		break;  /* MR7 */
	}
_adios:
	return;
}

void
Parser::assignmentTail(int *_retsignal, calc::StatementBlock *inclIn,
               const calc::WriteInfo& w,
                     calc::UsePar&   par )
{
#line 759 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr ass=NULL, theId=NULL, methodId=NULL, fid2=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 762 "calcparser.g"
	calc::FieldExprArgs args;
	if ( (LA(1)==TOK_IS) ) {
#line 763 "calcparser.g"
		zzmatch_wsig(TOK_IS, _handler);
		 consume();
#line 765 "calcparser.g"
		{
			if ( (LA(1)==TOK_TIMEOUTPUT)
 ) {
#line 765 "calcparser.g"
				zzmatch_wsig(TOK_TIMEOUTPUT, _handler);
				 consume();
#line 765 "calcparser.g"
				zzmatch_wsig(TOK_LP, _handler);
				 consume();
#line 765 "calcparser.g"
				exprList(&_signal,  inclIn,args ); if (_signal) goto _handler;
#line 765 "calcparser.g"
				zzmatch_wsig(TOK_RP, _handler);
				
#line 766 "calcparser.g"
				
				inclIn->addStatement(new calc::Timeoutput(w,par,args));
 consume();
			}
			else {
				if ( (setwd6[LA(1)]&0x40) ) {
#line 769 "calcparser.g"
					 calc::FieldExpr *r_expr  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 770 "calcparser.g"
					
					inclIn->addStatement(
					new calc::Assignment(inclIn,w,par,r_expr));
				}
				else {
					if (_sva) _signal=NoViableAlt;
					else _signal=NoSemViableAlt;
					goto _handler;  /* MR7 */
				}
			}
		}
	}
	else {
		if ( (LA(1)==TOK_ASSOP) ) {
#line 775 "calcparser.g"
			zzmatch_wsig(TOK_ASSOP, _handler);
			
			ass = (ANTLRTokenPtr)LT(1);
 consume();
#line 775 "calcparser.g"
			 calc::FieldExpr *r_expr  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 776 "calcparser.g"
			
			const calc::Operator& op = tokenOp(ass);
			calc::FieldExprArgs args(2);
			try {
				args[0] = new calc::FieldLeaf(par);
				args[1] = r_expr;
			} catch (...) {
				delete r_expr;
				throw;
			}
			calc::FieldExpr *newExpr= NEW_EXPR(ass,op,args);
			inclIn->addStatement(
			new calc::Assignment(inclIn,w,par,newExpr));
		}
		else {
			if ( (LA(1)==TOK_COMMA) ) {
#line 790 "calcparser.g"
				calc::FieldExprArgs args;
				std::vector<calc::UsePar> leftPars;
				leftPars.push_back(par);
#line 795 "calcparser.g"
				{
					int zzcnt=1;
					do {
#line 795 "calcparser.g"
						zzmatch_wsig(TOK_COMMA, _handler);
						 consume();
#line 795 "calcparser.g"
						 calc::UsePar par2  = parWithIndeces(&_signal, inclIn ); if (_signal) goto _handler;

#line 796 "calcparser.g"
						
						leftPars.push_back(par2);
					} while ( (LA(1)==TOK_COMMA) );
				}
#line 800 "calcparser.g"
				zzmatch_wsig(TOK_IS, _handler);
				 consume();
#line 801 "calcparser.g"
				zzmatch_wsig(TOK_ID, _handler);
				
				theId = (ANTLRTokenPtr)LT(1);
 consume();
#line 818 "calcparser.g"
				{
					if ( (LA(1)==TOK_2COL)
 ) {
#line 804 "calcparser.g"
						{
#line 804 "calcparser.g"
							zzmatch_wsig(TOK_2COL, _handler);
							 consume();
#line 805 "calcparser.g"
							zzmatch_wsig(TOK_ID, _handler);
							
							methodId = (ANTLRTokenPtr)LT(1);

#line 806 "calcparser.g"
							calc::FieldExprArgs args;
							calc::Symbol modelName = symbol(theId);
							std::string strArg;
 consume();
#line 810 "calcparser.g"
							modelLinkArgs(&_signal,  inclIn,strArg,args ); if (_signal) goto _handler;
#line 811 "calcparser.g"
							inclIn->addStatement(new
							calc::ModelLinkMethodStatement(
							inclIn,w,
							symbol(theId),symbol(methodId),
							leftPars, strArg,args));
						}
					}
					else {
						if ( (LA(1)==TOK_COMMA) ) {
#line 820 "calcparser.g"
							{
#line 820 "calcparser.g"
								zzmatch_wsig(TOK_COMMA, _handler);
								 consume();
#line 821 "calcparser.g"
								zzmatch_wsig(TOK_ID, _handler);
								
								fid2 = (ANTLRTokenPtr)LT(1);

#line 822 "calcparser.g"
								calc::Symbol sym1 = symbol(theId);
								const calc::Operator& op1 = calc::funcName2op(sym1.name());
								if (op1.opCode() == OP_NOP) notAFunc(sym1);
								calc::Symbol sym2 = symbol(fid2);
								const calc::Operator& op2 = calc::funcName2op(sym2.name());
								if (op2.opCode() == OP_NOP) notAFunc(sym2);
 consume();
#line 829 "calcparser.g"
								zzmatch_wsig(TOK_LP, _handler);
								 consume();
#line 829 "calcparser.g"
								exprList(&_signal,  inclIn,args ); if (_signal) goto _handler;
#line 829 "calcparser.g"
								zzmatch_wsig(TOK_RP, _handler);
								
#line 830 "calcparser.g"
								
								// CW TODO check if leftPars.size() == 1
								// syntax error otherwise
								inclIn->addStatement(new
								calc::DoubleAssignment(inclIn,leftPars[0],w,
								leftPars[0],leftPars[1],sym1,op1,op2,args));
								// can not do this in the syntax anymore
								// WrongDoubleAssignment(func.pos,*(func.op),*(f2.op))
 consume();
							}
						}
						else {
							if (_sva) _signal=NoViableAlt;
							else _signal=NoSemViableAlt;
							goto _handler;  /* MR7 */
						}
					}
				}
			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x80);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::statementSep(int *_retsignal)
{
#line 843 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_SC) ) {
#line 843 "calcparser.g"
		zzmatch_wsig(TOK_SC, _handler);
		 consume();
	}
	else {
		if ( (LA(1)==TOK_EOF) ) {
#line 844 "calcparser.g"
			zzmatch_wsig(TOK_EOF, _handler);
			 consume();
		}
		else {
			if (_sva) _signal=NoViableAlt;
			else _signal=NoSemViableAlt;
			goto _handler;  /* MR7 */
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x1);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 calc::FieldExpr * 
Parser::expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 851 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 852 "calcparser.g"
	calc::FieldExpr *right;
#line 853 "calcparser.g"
	 _retv  = xor_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 861 "calcparser.g"
	{
		while ( (LA(1)==TOK_OR) ) {
#line 855 "calcparser.g"
			zzmatch_wsig(TOK_OR, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 855 "calcparser.g"
			 right  = xor_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 856 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			_retv = NEW_EXPR(opS,calc::major2op(OP_OR),args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x2);
	return _retv;
	/* exception handlers */
_handler:
	switch ( _signal ) {
	case NoSignal: break;  /* MR7 */
	default :
		/* see pcrcalc/test34 without this no cleaning? */;
		checkParseError();
		_signal=NoSignal;  /* MR7 */
		break;  /* MR7 */
	}
_adios:
	return _retv;
}

 calc::FieldExpr * 
Parser::xor_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 869 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 870 "calcparser.g"
	calc::FieldExpr *right;
#line 871 "calcparser.g"
	 _retv  = and_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 879 "calcparser.g"
	{
		while ( (LA(1)==TOK_XOR)
 ) {
#line 873 "calcparser.g"
			zzmatch_wsig(TOK_XOR, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 873 "calcparser.g"
			 right  = and_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 874 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			_retv = NEW_EXPR(opS,calc::major2op(OP_XOR),args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x4);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::and_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 881 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 882 "calcparser.g"
	calc::FieldExpr *right;
#line 883 "calcparser.g"
	 _retv  = eq_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 891 "calcparser.g"
	{
		while ( (LA(1)==TOK_AND) ) {
#line 885 "calcparser.g"
			zzmatch_wsig(TOK_AND, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 885 "calcparser.g"
			 right  = eq_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 886 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			_retv = NEW_EXPR(opS,calc::major2op(OP_AND),args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x8);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::eq_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 894 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 895 "calcparser.g"
	calc::FieldExpr *right;
#line 896 "calcparser.g"
	 _retv  = comp_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 904 "calcparser.g"
	{
		while ( (LA(1)==TOK_EQ) ) {
#line 898 "calcparser.g"
			zzmatch_wsig(TOK_EQ, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 898 "calcparser.g"
			 right  = comp_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 899 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			_retv = NEW_EXPR(opS,tokenOp(opS),args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x10);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::comp_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 906 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 907 "calcparser.g"
	calc::FieldExpr *right;
#line 908 "calcparser.g"
	 _retv  = add_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 916 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMP) ) {
#line 910 "calcparser.g"
			zzmatch_wsig(TOK_COMP, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 910 "calcparser.g"
			 right  = add_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 911 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			_retv = NEW_EXPR(opS,tokenOp(opS),args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x20);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::add_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 918 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 919 "calcparser.g"
	calc::FieldExpr *right;
#line 921 "calcparser.g"
	 _retv  = mult_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 931 "calcparser.g"
	{
		while ( (setwd7[LA(1)]&0x40) ) {
#line 923 "calcparser.g"
			zzsetmatch_wsig(ADD_GROUP_set, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 923 "calcparser.g"
			 right  = mult_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 924 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			const calc::Operator& op = calc::major2op(
			opS->getType() == TOK_PLUS ? OP_BADD:OP_BMIN);
			_retv = NEW_EXPR(opS,op,args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x80);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::mult_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 933 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 934 "calcparser.g"
	calc::FieldExpr *right;
#line 935 "calcparser.g"
	 _retv  = pow_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 943 "calcparser.g"
	{
		while ( (setwd8[LA(1)]&0x1)
 ) {
#line 937 "calcparser.g"
			zzsetmatch_wsig(MULT_GROUP_set, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 937 "calcparser.g"
			 right  = pow_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 938 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			_retv   = NEW_EXPR(opS,tokenOp(opS),args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd8, 0x2);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::pow_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 945 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 946 "calcparser.g"
	calc::FieldExpr *right;
#line 947 "calcparser.g"
	 _retv  = sign_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 955 "calcparser.g"
	{
		while ( (LA(1)==TOK_POW) ) {
#line 949 "calcparser.g"
			zzmatch_wsig(TOK_POW, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 949 "calcparser.g"
			 right  = sign_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 950 "calcparser.g"
			calc::FieldExprArgs args(2);
			args[0] = _retv;
			args[1] = right;
			_retv = NEW_EXPR(opS,calc::major2op(OP_POW),args);
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd8, 0x4);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::sign_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 957 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr s=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 958 "calcparser.g"
	std::vector<calc::Symbol> signs;
#line 962 "calcparser.g"
	{
		while ( (setwd8[LA(1)]&0x8) && (setwd8[LA(2)]&0x10) ) {
#line 961 "calcparser.g"
			zzsetmatch_wsig(ADD_GROUP_set, _handler);
			
			s = (ANTLRTokenPtr)LT(1);

#line 961 "calcparser.g"
			signs.push_back(symbol(s));
 consume();
		}
	}
#line 962 "calcparser.g"
	 _retv  = not_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 963 "calcparser.g"
	
	/* solve Nominal/Ordinal not negative here
	something like this
	if ($ret->isConstant()) {
		Constant *c;
		for(size_t i = 0; i < signs.size(); ++i)
		if (signs[i].name() == "-")
		c->setValue(-c->value();
	} else  */
	for(int i = (int)(signs.size()-1);i>=0; i--) {
		calc::FieldExprArgs args(1);
		args[0] = _retv;
		const calc::Operator& op = calc::major2op(
		signs[i].name() == "+" ? OP_UADD:OP_UMIN);
		_retv = new calc::BranchExprImpl(signs[i],op,args);
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd8, 0x20);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::not_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 981 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr s=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
#line 982 "calcparser.g"
	std::vector<calc::Symbol> signs;
#line 985 "calcparser.g"
	{
		while ( (LA(1)==TOK_NOT) ) {
#line 984 "calcparser.g"
			zzmatch_wsig(TOK_NOT, _handler);
			
			s = (ANTLRTokenPtr)LT(1);

#line 984 "calcparser.g"
			signs.push_back(symbol(s));
 consume();
		}
	}
#line 985 "calcparser.g"
	 _retv  = misc_expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 986 "calcparser.g"
	
	for(int i = (int)(signs.size()-1);i>=0; i--) {
		calc::FieldExprArgs args(1);
		args[0] = _retv;
		_retv = new calc::BranchExprImpl(signs[i],calc::major2op(OP_NOT),args);
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd8, 0x40);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::FieldExpr * 
Parser::misc_expr(int *_retsignal, calc::StatementBlock *inclIn )
{
	 calc::FieldExpr * 	 _retv;
#line 996 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL, convF=NULL, lookF=NULL, tssF=NULL, r=NULL, indF=NULL, theId=NULL, methodId=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::FieldExpr * 	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_LP) ) {
#line 997 "calcparser.g"
		zzmatch_wsig(TOK_LP, _handler);
		 consume();
#line 997 "calcparser.g"
		 _retv  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 997 "calcparser.g"
		zzmatch_wsig(TOK_RP, _handler);
		 consume();
	}
	else {
		if ( (LA(1)==TOK_IF)
 ) {
#line 998 "calcparser.g"
			MAJOR_CODE c=OP_IF; calc::FieldExpr *f;
			calc::FieldExprArgs args;
#line 1001 "calcparser.g"
			zzmatch_wsig(TOK_IF, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 1001 "calcparser.g"
			zzmatch_wsig(TOK_LP, _handler);
			 consume();
#line 1001 "calcparser.g"
			 f  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1001 "calcparser.g"
			args.push_back(f);
#line 1002 "calcparser.g"
			zzsetmatch_wsig(THEN_GROUP_set, _handler);
			 consume();
#line 1002 "calcparser.g"
			 f  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1002 "calcparser.g"
			args.push_back(f);
#line 1003 "calcparser.g"
			{
				if ( (setwd8[LA(1)]&0x80) ) {
#line 1003 "calcparser.g"
					zzsetmatch_wsig(ELSE_GROUP_set, _handler);
					 consume();
#line 1003 "calcparser.g"
					 f  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1003 "calcparser.g"
					c=OP_IF_ELSE; args.push_back(f);
				}
			}
#line 1004 "calcparser.g"
			zzmatch_wsig(TOK_RP, _handler);
			
#line 1005 "calcparser.g"
			
			_retv = NEW_EXPR(opS,calc::major2op(c),args);
 consume();
		}
		else {
			if ( (LA(1)==TOK_CONV_F) ) {
#line 1008 "calcparser.g"
				zzmatch_wsig(TOK_CONV_F, _handler);
				
				convF = (ANTLRTokenPtr)LT(1);
 consume();
#line 1008 "calcparser.g"
				zzmatch_wsig(TOK_LP, _handler);
				 consume();
#line 1010 "calcparser.g"
				{
#line 1010 "calcparser.g"
					calc::Symbol nr;
					if ( (setwd9[LA(1)]&0x1) && (setwd9[LA(2)]&0x2) ) {
#line 1011 "calcparser.g"
						 nr  = number(&_signal); if (_signal) goto _handler;

#line 1011 "calcparser.g"
						zzmatch_wsig(TOK_RP, _handler);
						
#line 1012 "calcparser.g"
						_retv = new calc::Constant(createCastedConstant(convF,nr));
 consume();
					}
					else {
						if ( (setwd9[LA(1)]&0x4) && (setwd9[LA(2)]&0x8) ) {
#line 1014 "calcparser.g"
							calc::FieldExpr *e;
#line 1015 "calcparser.g"
							 e  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1015 "calcparser.g"
							zzmatch_wsig(TOK_RP, _handler);
							
#line 1016 "calcparser.g"
							
							calc::FieldExprArgs args(1);
							args[0] = e;
							_retv = NEW_EXPR(convF,tokenOp(convF),args);
 consume();
						}
						else {
							if (_sva) _signal=NoViableAlt;
							else _signal=NoSemViableAlt;
							goto _handler;  /* MR7 */
						}
					}
				}
			}
			else {
				if ( (LA(1)==TOK_LOOKUP_F)
 ) {
#line 1022 "calcparser.g"
					calc::FieldExprArgs args;
#line 1023 "calcparser.g"
					zzmatch_wsig(TOK_LOOKUP_F, _handler);
					
					lookF = (ANTLRTokenPtr)LT(1);
 consume();
#line 1024 "calcparser.g"
					zzmatch_wsig(TOK_LP, _handler);
					 consume();
#line 1024 "calcparser.g"
					 calc::UsePar table  = parWithIndeces(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1025 "calcparser.g"
					zzmatch_wsig(TOK_COMMA, _handler);
					 consume();
#line 1025 "calcparser.g"
					exprList(&_signal,  inclIn,args ); if (_signal) goto _handler;
#line 1026 "calcparser.g"
					zzmatch_wsig(TOK_RP, _handler);
					
#line 1027 "calcparser.g"
					
					try {
						_retv =
						new calc::LookupExpr(element(lookF),tokenOp(lookF),table,args);
					} catch (...) {
						calc::cleanUp(args);
						throw;
					}
 consume();
				}
				else {
					if ( (LA(1)==TOK_TIMEIN_F) ) {
#line 1036 "calcparser.g"
						calc::FieldExprArgs args;
#line 1037 "calcparser.g"
						zzmatch_wsig(TOK_TIMEIN_F, _handler);
						
						tssF = (ANTLRTokenPtr)LT(1);
 consume();
#line 1038 "calcparser.g"
						zzmatch_wsig(TOK_LP, _handler);
						 consume();
#line 1038 "calcparser.g"
						 calc::UsePar tss  = parWithIndeces(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1038 "calcparser.g"
						zzmatch_wsig(TOK_COMMA, _handler);
						 consume();
#line 1039 "calcparser.g"
						exprList(&_signal,  inclIn,args ); if (_signal) goto _handler;
#line 1039 "calcparser.g"
						zzmatch_wsig(TOK_RP, _handler);
						
#line 1040 "calcparser.g"
						
						try {
							// pcrcalc/test37
							if (!inclIn->inDynamic())
							errorFuncInDynamic(symbol(tssF));
							_retv =
							new calc::TimeinputExpr(element(tssF),tokenOp(tssF),tss,args);
						} catch (...) {
							calc::cleanUp(args);
							throw;
						}
 consume();
					}
					else {
						if ( (setwd9[LA(1)]&0x10) ) {
#line 1052 "calcparser.g"
							calc::Symbol nr;
#line 1053 "calcparser.g"
							 nr  = number(&_signal); if (_signal) goto _handler;

#line 1054 "calcparser.g"
							_retv = new calc::Constant(nr);
						}
						else {
							if ( (LA(1)==TOK_REFERENCE) ) {
#line 1056 "calcparser.g"
								zzmatch_wsig(TOK_REFERENCE, _handler);
								
								r = (ANTLRTokenPtr)LT(1);

#line 1057 "calcparser.g"
								
								calc::UsePar cpar(calc::ConstructPar(inclIn,symbol(r)));
								_retv = new calc::FieldLeaf(cpar);
 consume();
							}
							else {
								if ( (LA(1)==TOK_TIMEINPUT) ) {
#line 1061 "calcparser.g"
									calc::Symbol stackSuffix;
#line 1062 "calcparser.g"
									zzmatch_wsig(TOK_TIMEINPUT, _handler);
									
									indF = (ANTLRTokenPtr)LT(1);
 consume();
#line 1063 "calcparser.g"
									zzmatch_wsig(TOK_LP, _handler);
									 consume();
#line 1065 "calcparser.g"
									{
										if ( (setwd9[LA(1)]&0x20)
 ) {
#line 1065 "calcparser.g"
											zzsetmatch_wsig(ID_GROUP_set, _handler);
											
#line 1066 "calcparser.g"
											// pcrcalc/test257
											expectFile("map stack");
 consume();
										}
									}
#line 1070 "calcparser.g"
									 stackSuffix  = qid(&_signal); if (_signal) goto _handler;

#line 1072 "calcparser.g"
									{
										if ( (setwd9[LA(1)]&0x40) ) {
#line 1072 "calcparser.g"
											zzsetmatch_wsig(err7, _handler);
											
#line 1073 "calcparser.g"
											// pcrcalc/test257a
											expectFile("map stack");
 consume();
										}
									}
#line 1077 "calcparser.g"
									zzmatch_wsig(TOK_RP, _handler);
									
#line 1078 "calcparser.g"
									
									calc::Symbol fSym(symbol(indF));
									const calc::Operator& fOp = calc::funcName2op(fSym.name());
									if (!inclIn->inDynamic())
									errorFuncInDynamic(fSym);
									_retv = new calc::StackInput(fSym, calc::BindedSymbol(stackSuffix),
									fOp.opCode()==OP_TIMEINPUTSPARSE);
 consume();
								}
								else {
									if ( (LA(1)==TOK_ID) ) {
#line 1086 "calcparser.g"
										zzmatch_wsig(TOK_ID, _handler);
										
										theId = (ANTLRTokenPtr)LT(1);
 consume();
#line 1088 "calcparser.g"
										{
											if ( (LA(1)==TOK_LP) ) {
#line 1088 "calcparser.g"
												zzmatch_wsig(TOK_LP, _handler);
												
#line 1089 "calcparser.g"
												calc::FieldExprArgs args;
												calc::Symbol name = symbol(theId);
												const calc::Operator& o = calc::funcName2op(name.name());
												if (o.opCode() == OP_NOP)
												notAFunc(name);
												if (!inclIn->inDynamic())
												switch(o.opCode()) {
													case OP_TIMEINPUT:
													case OP_TIME:
													case OP_TIMESLICE:
													errorFuncInDynamic(name);
													default: ;
												}
 consume();
#line 1103 "calcparser.g"
												{
													if ( (setwd9[LA(1)]&0x80) ) {
#line 1103 "calcparser.g"
														exprList(&_signal,  inclIn,args ); if (_signal) goto _handler;
													}
												}
#line 1104 "calcparser.g"
												zzmatch_wsig(TOK_RP, _handler);
												
#line 1105 "calcparser.g"
												
												_retv = new calc::BranchExprImpl(name,o,args);
 consume();
											}
											else {
												if ( (LA(1)==TOK_2COL)
 ) {
#line 1108 "calcparser.g"
													zzmatch_wsig(TOK_2COL, _handler);
													 consume();
#line 1109 "calcparser.g"
													zzmatch_wsig(TOK_ID, _handler);
													
													methodId = (ANTLRTokenPtr)LT(1);

#line 1110 "calcparser.g"
													calc::FieldExprArgs args;
													calc::Symbol modelName = symbol(theId);
													std::string strArg;
 consume();
#line 1114 "calcparser.g"
													modelLinkArgs(&_signal,  inclIn,strArg,args ); if (_signal) goto _handler;
#line 1115 "calcparser.g"
													_retv = new calc::ModelLinkMethodExpr(modelName,symbol(methodId),
													strArg,args);
												}
												else {
													if ( (setwd10[LA(1)]&0x1) ) {
#line 1119 "calcparser.g"
														
														calc::ConstructPar p(inclIn,symbol(theId));
#line 1122 "calcparser.g"
														{
															while ( (LA(1)==TOK_LB) ) {
#line 1122 "calcparser.g"
																 calc::Symbol a  = arrayIndex(&_signal); if (_signal) goto _handler;

#line 1122 "calcparser.g"
																p.d_index.push_back(a);
															}
														}
#line 1123 "calcparser.g"
														
														calc::UsePar pp(p);
														_retv = new calc::FieldLeaf(pp);
													}
													else {
														if (_sva) _signal=NoViableAlt;
														else _signal=NoSemViableAlt;
														goto _handler;  /* MR7 */
													}
												}
											}
										}
									}
									else {
										if (_sva) _signal=NoViableAlt;
										else _signal=NoSemViableAlt;
										goto _handler;  /* MR7 */
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd10, 0x2);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

void
Parser::modelLinkArgs(int *_retsignal, calc::StatementBlock *inclIn, std::string& strArg, calc::FieldExprArgs& args )
{
#line 1130 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr ref=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 1131 "calcparser.g"
	zzmatch_wsig(TOK_LP, _handler);
	 consume();
#line 1132 "calcparser.g"
	{
		if ( (LA(1)==TOK_REFERENCE) && (setwd10[LA(2)]&0x4) ) {
#line 1132 "calcparser.g"
			zzmatch_wsig(TOK_REFERENCE, _handler);
			
			ref = (ANTLRTokenPtr)LT(1);

#line 1132 "calcparser.g"
			strArg = symbol(ref).name();
 consume();
#line 1133 "calcparser.g"
			{
				if ( (LA(1)==TOK_COMMA) ) {
#line 1133 "calcparser.g"
					zzmatch_wsig(TOK_COMMA, _handler);
					 consume();
				}
			}
		}
	}
#line 1135 "calcparser.g"
	{
		if ( (setwd10[LA(1)]&0x8)
 ) {
#line 1135 "calcparser.g"
			exprList(&_signal,  inclIn,args ); if (_signal) goto _handler;
		}
	}
#line 1136 "calcparser.g"
	zzmatch_wsig(TOK_RP, _handler);
	 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd10, 0x10);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::exprList(int *_retsignal, calc::StatementBlock *inclIn, calc::FieldExprArgs& args )
{
#line 1140 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 1141 "calcparser.g"
	calc::FieldExpr *e;
#line 1142 "calcparser.g"
	 e  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1142 "calcparser.g"
	args.push_back(e);
#line 1144 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA) ) {
#line 1143 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 1143 "calcparser.g"
			 e  = expr(&_signal,  inclIn ); if (_signal) goto _handler;

#line 1143 "calcparser.g"
			args.push_back(e);
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd10, 0x20);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 calc::ConstructPar  
Parser::parWithIndeces(int *_retsignal, calc::StatementBlock *s )
{
	 calc::ConstructPar  	 _retv;
#line 1150 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr i=NULL, r=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::ConstructPar  	))
	*_retsignal = NoSignal;
#line 1151 "calcparser.g"
	calc::Symbol a; _retv.d_block=s;
#line 1153 "calcparser.g"
	{
		if ( (LA(1)==TOK_ID) ) {
#line 1153 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			i = (ANTLRTokenPtr)LT(1);

#line 1153 "calcparser.g"
			_retv.d_name = symbol(i);
 consume();
#line 1155 "calcparser.g"
			{
				while ( (LA(1)==TOK_LB) ) {
#line 1154 "calcparser.g"
					 a  = arrayIndex(&_signal); if (_signal) goto _handler;

#line 1154 "calcparser.g"
					_retv.d_index.push_back(a);
				}
			}
		}
		else {
			if ( (LA(1)==TOK_REFERENCE) ) {
#line 1156 "calcparser.g"
				zzmatch_wsig(TOK_REFERENCE, _handler);
				
				r = (ANTLRTokenPtr)LT(1);

#line 1156 "calcparser.g"
				_retv.d_name = symbol(r);
 consume();
			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd10, 0x40);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Symbol  
Parser::arrayIndex(int *_retsignal)
{
	 calc::Symbol  	 _retv;
#line 1160 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr id=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Symbol  	))
	*_retsignal = NoSignal;
#line 1161 "calcparser.g"
	zzmatch_wsig(TOK_LB, _handler);
	 consume();
#line 1161 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	id = (ANTLRTokenPtr)LT(1);

#line 1161 "calcparser.g"
	_retv = symbol(id);
 consume();
#line 1161 "calcparser.g"
	zzmatch_wsig(TOK_RB, _handler);
	 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd10, 0x80);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Symbol  
Parser::qid(int *_retsignal)
{
	 calc::Symbol  	 _retv;
#line 1165 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr i=NULL, r=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Symbol  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_ID)
 ) {
#line 1165 "calcparser.g"
		zzmatch_wsig(TOK_ID, _handler);
		
		i = (ANTLRTokenPtr)LT(1);

#line 1165 "calcparser.g"
		_retv = symbol(i);
 consume();
	}
	else {
		if ( (LA(1)==TOK_REFERENCE) ) {
#line 1166 "calcparser.g"
			zzmatch_wsig(TOK_REFERENCE, _handler);
			
			r = (ANTLRTokenPtr)LT(1);

#line 1166 "calcparser.g"
			_retv = symbol(r);
 consume();
		}
		else {
			if (_sva) _signal=NoViableAlt;
			else _signal=NoSemViableAlt;
			goto _handler;  /* MR7 */
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd11, 0x1);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Symbol  
Parser::number(int *_retsignal)
{
	 calc::Symbol  	 _retv;
#line 1172 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Symbol  	))
	*_retsignal = NoSignal;
#line 1173 "calcparser.g"
	calc::Symbol signS,valS;
#line 1174 "calcparser.g"
	{
		if ( (setwd11[LA(1)]&0x2) ) {
#line 1174 "calcparser.g"
			 signS  = sign(&_signal); if (_signal) goto _handler;

		}
	}
#line 1176 "calcparser.g"
	 valS  = unsignedNumber(&_signal); if (_signal) goto _handler;

#line 1177 "calcparser.g"
	
	if (signS.empty())
	_retv = valS;
	else {
		// TODO er zouden spaties tussen signS en valS kunnen zitten!
		signS.setName(signS.name()+valS.name());
		_retv = signS;
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd11, 0x4);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Symbol  
Parser::unsignedNumber(int *_retsignal)
{
	 calc::Symbol  	 _retv;
#line 1188 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr v1=NULL, v2=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Symbol  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_INT) ) {
#line 1189 "calcparser.g"
		zzmatch_wsig(TOK_INT, _handler);
		
		v1 = (ANTLRTokenPtr)LT(1);

#line 1190 "calcparser.g"
		_retv = symbol(v1);
 consume();
	}
	else {
		if ( (LA(1)==TOK_FLOAT) ) {
#line 1191 "calcparser.g"
			zzmatch_wsig(TOK_FLOAT, _handler);
			
			v2 = (ANTLRTokenPtr)LT(1);

#line 1192 "calcparser.g"
			_retv = symbol(v2);
 consume();
		}
		else {
			if (_sva) _signal=NoViableAlt;
			else _signal=NoSemViableAlt;
			goto _handler;  /* MR7 */
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd11, 0x8);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Symbol  
Parser::sign(int *_retsignal)
{
	 calc::Symbol  	 _retv;
#line 1194 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr v1=NULL, v2=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Symbol  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_PLUS)
 ) {
#line 1195 "calcparser.g"
		zzmatch_wsig(TOK_PLUS, _handler);
		
		v1 = (ANTLRTokenPtr)LT(1);

#line 1196 "calcparser.g"
		_retv =symbol(v1);
 consume();
	}
	else {
		if ( (LA(1)==TOK_MINUS) ) {
#line 1198 "calcparser.g"
			zzmatch_wsig(TOK_MINUS, _handler);
			
			v2 = (ANTLRTokenPtr)LT(1);

#line 1199 "calcparser.g"
			_retv =symbol(v2);
 consume();
		}
		else {
			if (_sva) _signal=NoViableAlt;
			else _signal=NoSemViableAlt;
			goto _handler;  /* MR7 */
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd11, 0x10);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}
