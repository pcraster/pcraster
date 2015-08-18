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
#ifdef _MSC_VER
#pragma warning( disable:4127 ) // warning C4127: conditional expression is constant
#pragma warning( disable:4102 ) // warning C4102: '_adios' : unreferenced label
#pragma warning( disable:4189 ) // warning C4189: 'zzpf' : local variable is initialized but not referenced
#pragma warning( disable:4700 ) // warning C4700: uninitialized local variable '_retv' used
#pragma warning( disable:4702 ) // warning C4702: unreachable code
#endif

#ifdef BORLANDC
#pragma warn -8004
#endif

#define ANTLR_VERSION	13319
#include "pcctscfg.h"
#include "pccts_stdio.h"
#line 1 "calcparser.g"
#include "tokens.h"

#include "stddefx.h"
#include <math.h>
#include <string>
#include <vector>

// see known problems M19:
#define PURIFY(r,s)

// all that is in return stat's
// to Parser.h compiling
namespace calc {
class ASTNumber;
class ASTId;
class ASTNodeVector;
class ASTNodeList;
class  ASTNode;
class  ASTPar;
class  ASTExpr;
class  ASTStat;
class  ASTAss;

  class  ASTScript;

  struct ParsReportMoment;
class  Position;
class  Operator;
class  RunSettings;
}

#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif
#ifndef INCLUDED_CALC_ID
#include "calc_id.h"
#define INCLUDED_CALC_ID
#endif
#ifndef INCLUDED_CALC_POSITION
#include "calc_position.h" // setPriority
#define INCLUDED_CALC_POSITION
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
#line 66 "calcparser.g"


#ifndef INCLUDED_MISC
#include "misc.h"
#define INCLUDED_MISC
#endif
#ifndef INCLUDED_CALC_RUNSETTINGS
#include "calc_runsettings.h"
#define INCLUDED_CALC_RUNSETTINGS
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_CALC_ASTDEFINITION
#include "calc_astdefinition.h"
#define INCLUDED_CALC_ASTDEFINITION
#endif
#ifndef INCLUDED_CALC_ASTNODEVECTOR
#include "calc_astnodevector.h"
#define INCLUDED_CALC_ASTNODEVECTOR
#endif
#ifndef INCLUDED_CALC_ASTNODELIST
#include "calc_astnodelist.h"
#define INCLUDED_CALC_ASTNODELIST
#endif
#ifndef INCLUDED_CALC_NONASSEXPR
#include "calc_nonassexpr.h"
#define INCLUDED_CALC_NONASSEXPR
#endif
#ifndef INCLUDED_CALC_ASTEXPR
#include "calc_astexpr.h"
#define INCLUDED_CALC_ASTEXPR
#endif
#ifndef INCLUDED_CALC_LINKINEXPR
#include "calc_linkinexpr.h"
#define INCLUDED_CALC_LINKINEXPR
#endif
#ifndef INCLUDED_CALC_ASTNUMBER
#include "calc_astnumber.h"
#define INCLUDED_CALC_ASTNUMBER
#endif
#ifndef INCLUDED_CALC_OPERATOR
#include "calc_operator.h"
#define INCLUDED_CALC_OPERATOR
#endif
#ifndef INCLUDED_CALC_ASTPAR
#include "calc_astpar.h"
#define INCLUDED_CALC_ASTPAR
#endif
#ifndef INCLUDED_CALC_ASTASS
#include "calc_astass.h"
#define INCLUDED_CALC_ASTASS
#endif
#ifndef INCLUDED_CALC_ASTSTAT
#include "calc_aststat.h"
#define INCLUDED_CALC_ASTSTAT
#endif
#ifndef INCLUDED_CALC_DYNAMICSECTION
#include "calc_dynamicsection.h"
#define INCLUDED_CALC_DYNAMICSECTION
#endif
#ifndef INCLUDED_CALC_REPEATUNTIL
#include "calc_repeatuntil.h"
#define INCLUDED_CALC_REPEATUNTIL
#endif
#ifndef INCLUDED_CALC_IDLIST
#include "calc_idlist.h"
#define INCLUDED_CALC_IDLIST
#endif
#ifndef INCLUDED_CALC_ASTSCRIPT
#include "calc_astscript.h"
#define INCLUDED_CALC_ASTSCRIPT
#endif
/*
#ifndef  INCLUDED_CALC_INDEXTABLE
#include "calc_indextable.h"
#define  INCLUDED_CALC_INDEXTABLE
#endif
#ifndef INCLUDED_CALC_ARRAYDEFINITION
#include "calc_arraydefinition.h"
#define INCLUDED_CALC_ARRAYDEFINITION
#endif
#ifndef INCLUDED_CALC_ARRAYDEFVECTOR
#include "calc_arraydefvector.h"
#define INCLUDED_CALC_ARRAYDEFVECTOR
#endif
#ifndef INCLUDED_CALC_FOREACH
#include "calc_foreach.h"
#define INCLUDED_CALC_FOREACH
#endif
#ifndef INCLUDED_CALC_STDOUTSTAT
#include "calc_stdoutstat.h"
#define INCLUDED_CALC_STDOUTSTAT
#endif
#ifndef INCLUDED_CALC_WRITEINFO
#include "calc_writeinfo.h"
#define INCLUDED_CALC_WRITEINFO
#endif
*/

#ifndef INCLUDED_CALC_FINDSYMBOL
#include "calc_findsymbol.h"
#define INCLUDED_CALC_FINDSYMBOL
#endif
#ifndef INCLUDED_CALC_REPORT
#include "calc_report.h"
#define INCLUDED_CALC_REPORT
#endif

#ifndef INCLUDED_LEXGRAMMAR
#include "lexgrammar.h"
#define INCLUDED_LEXGRAMMAR
#endif

using namespace calc;

typedef calc::LexToken ANTLRToken;

void Parser::notImplemented(const ANTLRTokenPtr at) const
{
	genId(at).posError("Sorry, this language construct is not implemented");
}

calc::Id Parser::genId(const ANTLRTokenPtr at) const
{
	return calc::Id(mytoken(at)->stringVal(),mytoken(at)->position());
};

const calc::Position* Parser::position(const ANTLRTokenPtr at) const
{
	return mytoken(at)->position();
}

calc::ASTExpr* Parser::createExpr(const Position       *pos,
const calc::Operator *op) const
{
	PRECOND(op);
	return new calc::ASTExpr(pos,*op);
}

calc::ASTExpr* Parser::createExpr(const ANTLRTokenPtr   at,
const calc::Operator *op) const
{
	PRECOND(op);
	return new calc::ASTExpr(mytoken(at)->position(),*op);
}

const calc::Operator* Parser::tokenOp(const ANTLRTokenPtr at) const
{
	PRECOND(calc::major2op(mytoken(at)->opCode()));
	return calc::major2op(mytoken(at)->opCode());
}

//! current token saus this is an exp where we expect a file
void Parser::expectFile(const std::string& fileDescr) {
	calc::Id id(genId(LT(1)));
	id.posError("Expression illegal here, expecting name of "+fileDescr);
}

void Parser::checkParseError() {
	const ANTLRTokenPtr at(LT(1));
	calc::Id pe(genId(at));
	std::ostringstream msg;
	if (mytoken(at)->stringVal().empty()) // no keyword or symbol end-of-input
	msg << "Syntax error: more characters expected at this point";
	else {
		bool kw=mytoken(at)->isKeyword();
		// pcrcalc/test34
		msg  << "Syntax error at " << (kw ? "keyword ":"symbol ") << pe.qName();
		if (kw)
		msg  << "\n Keywords can not be used as names for files or variables"
		<< "\n Keywords must be placed in a specific order";
	}
	pe.posError(msg);
}


void Parser::illegalFunc(const calc::ASTPar& s) const {
	// pcrcalc/test41
	s.posError("function '"+s.name()+"' illegal here");
}

const calc::Operator* Parser::expectFunction(const calc::Id& id) const {
	const calc::Operator* o = calc::opName2op(id.name());
	if (!o)
	id.posError("'"+id.name()+"' is not a function");
	return o;
}

const calc::ASTPar& Parser::expectId(const calc::ASTPar& par) const {
	if (0 /*par.isArray() */)
	par.posError("Illegal construct");
	// return Id(par.name(),par.position());
	return par;
}

calc::ASTNumber* Parser::createCastedASTNumber(
const ANTLRTokenPtr   convF,
const calc::Id& nr) const
{
	calc::Id s(genId(convF));
	std::ostringstream name;
	name << s.name() << "(" << nr.name() << ")";
	s.setName(name.str());
	return new calc::ASTNumber(s, tokenOp(convF)->vs(),nr);
}

void Parser::
zzdflthandlers( int _signal, int *_retsignal )
{
	*_retsignal = _signal;
}


 std::auto_ptr<calc::ASTScript>  
Parser::model(int *_retsignal)
{
	 std::auto_ptr<calc::ASTScript>  	 _retv;
#line 406 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( std::auto_ptr<calc::ASTScript>  	))
	*_retsignal = NoSignal;
#line 407 "calcparser.g"
	_retv.reset(new ASTScript());
	AP_ASTNode b;
#line 410 "calcparser.g"
	{
		if ( (LA(1)==TOK_INTERFACE) ) {
#line 410 "calcparser.g"
			zzmatch_wsig(TOK_INTERFACE, _handler);
			 consume();
#line 410 "calcparser.g"
			interfaceSection(&_signal, _retv.get() ); if (_signal) goto _handler;
		}
	}
#line 411 "calcparser.g"
	{
		if ( (LA(1)==TOK_BINDING) ) {
#line 411 "calcparser.g"
			zzmatch_wsig(TOK_BINDING, _handler);
			 consume();
#line 411 "calcparser.g"
			bindingSection(&_signal, _retv.get() ); if (_signal) goto _handler;
		}
	}
#line 412 "calcparser.g"
	{
		if ( (LA(1)==TOK_AREAMAP) ) {
#line 412 "calcparser.g"
			areaMapSection(&_signal, _retv.get() ); if (_signal) goto _handler;
		}
	}
#line 412 "calcparser.g"
	{
		if ( (LA(1)==TOK_TIMER) ) {
#line 412 "calcparser.g"
			timer(&_signal, _retv.get() ); if (_signal) goto _handler;
		}
	}
#line 413 "calcparser.g"
	 b  = body(&_signal); if (_signal) goto _handler;

#line 413 "calcparser.g"
	zzmatch_wsig(TOK_EOF, _handler);
	
#line 414 "calcparser.g"
	
	if (!b.get())
	throw com::Exception("script contains no code");
	_retv->transferCode(b.release());
 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd1, 0x1);
	return _retv;
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
	return _retv;
}

void
Parser::externalBindings(int *_retsignal, calc::ASTNodeVector& l )
{
#line 429 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 430 "calcparser.g"
	AP_ASTNode e;
#line 434 "calcparser.g"
	{
		while ( (setwd1[LA(1)]&0x2)
 ) {
#line 431 "calcparser.g"
			 e  = bindingDefinition(&_signal); if (_signal) goto _handler;

#line 432 "calcparser.g"
			l.transferPushBack(e.release());
#line 433 "calcparser.g"
			{
				if ( (LA(1)==TOK_SC) ) {
#line 433 "calcparser.g"
					zzmatch_wsig(TOK_SC, _handler);
					 consume();
				}
			}
		}
	}
#line 434 "calcparser.g"
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
Parser::bindingSection(int *_retsignal, calc::ASTScript *script )
{
#line 441 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 442 "calcparser.g"
	AP_ASTNode e;
#line 449 "calcparser.g"
	{
		while ( (setwd1[LA(1)]&0x8) && (setwd1[LA(2)]&0x10) ) {
#line 443 "calcparser.g"
			 e  = bindingDefinition(&_signal); if (_signal) goto _handler;

#line 444 "calcparser.g"
			
			if (e.get())
			script->transferOneBinding(e.release());
			// else index/array stuf
#line 449 "calcparser.g"
			zzmatch_wsig(TOK_SC, _handler);
			 consume();
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd1, 0x20);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 Parser::AP_ASTNode  
Parser::bindingDefinition(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 452 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 453 "calcparser.g"
	AP_ASTNode e;
#line 454 "calcparser.g"
	 calc::ASTPar p  = parWithIndeces(&_signal); if (_signal) goto _handler;

#line 455 "calcparser.g"
	zzmatch_wsig(TOK_IS, _handler);
	 consume();
#line 455 "calcparser.g"
	 _retv  = bindingRHS(&_signal, p ); if (_signal) goto _handler;

	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd1, 0x40);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::bindingRHS(int *_retsignal, const calc::ASTPar& par )
{
	 Parser::AP_ASTNode  	 _retv;
#line 461 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr convF=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 462 "calcparser.g"
	AP_ASTNode n; calc::Id right;
#line 464 "calcparser.g"
	{
		if ( (setwd1[LA(1)]&0x80) ) {
#line 464 "calcparser.g"
			 right  = qid(&_signal); if (_signal) goto _handler;

#line 464 "calcparser.g"
			n.reset(new calc::ASTPar(right));
#line 465 "calcparser.g"
			{
				if ( (LA(1)==TOK_LP) ) {
#line 465 "calcparser.g"
					zzmatch_wsig(TOK_LP, _handler);
					
#line 465 "calcparser.g"
					illegalFunc(right);
 consume();
				}
			}
		}
		else {
			if ( (setwd2[LA(1)]&0x1)
 ) {
#line 466 "calcparser.g"
				 right  = number(&_signal); if (_signal) goto _handler;

#line 467 "calcparser.g"
				n.reset(new calc::ASTNumber(right));
			}
			else {
				if ( (LA(1)==TOK_CONV_F) ) {
#line 468 "calcparser.g"
					zzmatch_wsig(TOK_CONV_F, _handler);
					
					convF = (ANTLRTokenPtr)LT(1);
 consume();
#line 468 "calcparser.g"
					zzmatch_wsig(TOK_LP, _handler);
					 consume();
#line 468 "calcparser.g"
					 right  = number(&_signal); if (_signal) goto _handler;

#line 469 "calcparser.g"
					zzmatch_wsig(TOK_RP, _handler);
					
#line 472 "calcparser.g"
					n.reset(createCastedASTNumber(convF,right));
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
#line 474 "calcparser.g"
	
	if (n.get())
	_retv.reset(new ASTAss(expectId( par),n.get()));
	// else index/array stuff
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x2);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

void
Parser::interfaceSection(int *_retsignal, calc::ASTScript *script )
{
#line 481 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr id=NULL, key=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 494 "calcparser.g"
	{
		while ( (LA(1)==TOK_ID) && (LA(2)==TOK_LC) ) {
#line 482 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			id = (ANTLRTokenPtr)LT(1);
 consume();
#line 482 "calcparser.g"
			zzmatch_wsig(TOK_LC, _handler);
			
#line 483 "calcparser.g"
			// define here, to create new one each repition:
			ASTDefinition ib;
			ib.setName(genId(id));
 consume();
#line 490 "calcparser.g"
			{
#line 487 "calcparser.g"
				calc::Id value;
				while ( (LA(1)==TOK_ID) ) {
#line 488 "calcparser.g"
					zzmatch_wsig(TOK_ID, _handler);
					
					key = (ANTLRTokenPtr)LT(1);
 consume();
#line 488 "calcparser.g"
					zzmatch_wsig(TOK_IS, _handler);
					 consume();
#line 488 "calcparser.g"
					{
						if ( (setwd2[LA(1)]&0x4) ) {
#line 488 "calcparser.g"
							 value  = nrOrId(&_signal); if (_signal) goto _handler;

						}
					}
#line 488 "calcparser.g"
					zzmatch_wsig(TOK_SC, _handler);
					
#line 489 "calcparser.g"
					ib.add(genId(key),value);
 consume();
				}
			}
#line 491 "calcparser.g"
			zzmatch_wsig(TOK_RC, _handler);
			 consume();
#line 492 "calcparser.g"
			{
				if ( (LA(1)==TOK_SC)
 ) {
#line 492 "calcparser.g"
					zzmatch_wsig(TOK_SC, _handler);
					 consume();
				}
			}
#line 493 "calcparser.g"
			script->addInterfaceDefinition(ib);
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x8);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::areaMapSection(int *_retsignal, calc::ASTScript *script )
{
#line 497 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 497 "calcparser.g"
	AP_ASTId s;
#line 498 "calcparser.g"
	zzmatch_wsig(TOK_AREAMAP, _handler);
	 consume();
#line 498 "calcparser.g"
	 s  = nrOrPar(&_signal); if (_signal) goto _handler;

#line 498 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	
#line 499 "calcparser.g"
	script->transferAreaMap(s.release());
 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x10);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::timer(int *_retsignal, calc::ASTScript *script )
{
#line 502 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 503 "calcparser.g"
	AP_ASTId eTimerStartOrTss,eTimerEnd,eTimerStep;
#line 504 "calcparser.g"
	zzmatch_wsig(TOK_TIMER, _handler);
	 consume();
#line 505 "calcparser.g"
	 eTimerStartOrTss  = nrOrPar(&_signal); if (_signal) goto _handler;

#line 506 "calcparser.g"
	{
		if ( (setwd2[LA(1)]&0x20) ) {
#line 506 "calcparser.g"
			 eTimerEnd  = nrOrPar(&_signal); if (_signal) goto _handler;

#line 507 "calcparser.g"
			 eTimerStep  = nrOrPar(&_signal); if (_signal) goto _handler;

#line 508 "calcparser.g"
			
			script->transferTimerSection(
			eTimerStartOrTss.release(),
			eTimerEnd.release(),
			eTimerStep.release());
		}
	}
#line 514 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	 consume();
#line 515 "calcparser.g"
	{
		while ( (LA(1)==TOK_ID) && (LA(2)==TOK_IS) ) {
#line 515 "calcparser.g"
			reportMomentDef(&_signal, script ); if (_signal) goto _handler;
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd2, 0x40);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 Parser::AP_ASTNode  
Parser::body(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 519 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr td=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 520 "calcparser.g"
	AP_ASTNodeList initial;
	std::auto_ptr<DynamicSection> dynamic;
#line 525 "calcparser.g"
	{
		if ( (setwd2[LA(1)]&0x80) && (setwd3[LA(2)]&0x1) ) {
#line 525 "calcparser.g"
			{
				if ( (LA(1)==TOK_INITIAL) ) {
#line 525 "calcparser.g"
					zzmatch_wsig(TOK_INITIAL, _handler);
					 consume();
				}
				else {
					if ( (LA(1)==TOK_MODEL)
 ) {
#line 525 "calcparser.g"
						zzmatch_wsig(TOK_MODEL, _handler);
						 consume();
					}
				}
			}
#line 525 "calcparser.g"
			 initial  = statementList(&_signal); if (_signal) goto _handler;

		}
	}
#line 527 "calcparser.g"
	{
#line 527 "calcparser.g"
		AP_ASTNodeList dynamicStatements;
		if ( (LA(1)==TOK_DYNAMIC) ) {
#line 528 "calcparser.g"
			zzmatch_wsig(TOK_DYNAMIC, _handler);
			
			td = (ANTLRTokenPtr)LT(1);
 consume();
#line 528 "calcparser.g"
			 dynamicStatements  = statementList(&_signal); if (_signal) goto _handler;

#line 529 "calcparser.g"
			dynamic.reset(new calc::DynamicSection(
			position(td), dynamicStatements.release()));
		}
	}
#line 533 "calcparser.g"
	if (!dynamic.get()) {
		// no dynamic body is initial
		_retv=initial;
	} else {
		if (!initial.get()) {
			// no initial body is dynamic
			_retv=dynamic;
		} else { // both
			AP_ASTNodeList body(new calc::ASTNodeList());
			body->transferPushBack(initial.release());
			body->transferPushBack(dynamic.release());
			_retv=body;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x2);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

void
Parser::reportMomentDef(int *_retsignal, calc::ASTScript *script )
{
#line 554 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr reportId=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 555 "calcparser.g"
	std::vector<calc::ParsReportMoment> m;
#line 556 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	reportId = (ANTLRTokenPtr)LT(1);
 consume();
#line 557 "calcparser.g"
	zzmatch_wsig(TOK_IS, _handler);
	 consume();
#line 557 "calcparser.g"
	reportMoments(&_signal, m ); if (_signal) goto _handler;
#line 557 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	
#line 558 "calcparser.g"
	script->addReportDefinition(Report(genId(reportId),m));
 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x4);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::reportMoments(int *_retsignal, std::vector<calc::ParsReportMoment> &m )
{
#line 562 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 563 "calcparser.g"
	calc::ParsReportMoment p;
#line 564 "calcparser.g"
	reportMoment(&_signal, p ); if (_signal) goto _handler;
#line 565 "calcparser.g"
	m.push_back(p);
#line 568 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA) ) {
#line 566 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 566 "calcparser.g"
			reportMoment(&_signal, p ); if (_signal) goto _handler;
#line 567 "calcparser.g"
			m.push_back(p);
		}
	}
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x8);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::reportMoment(int *_retsignal, calc::ParsReportMoment& p )
{
#line 571 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr start=NULL, range=NULL, range2=NULL;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 572 "calcparser.g"
	p.start=0;  p.step=0; p.end=0;
#line 589 "calcparser.g"
	{
		if ( (LA(1)==TOK_INT) ) {
#line 574 "calcparser.g"
			{
#line 574 "calcparser.g"
				zzmatch_wsig(TOK_INT, _handler);
				
				start = (ANTLRTokenPtr)LT(1);

#line 574 "calcparser.g"
				p.start = mytoken(start)->integerVal();
 consume();
#line 577 "calcparser.g"
				{
					if ( (LA(1)==TOK_PLUS) ) {
#line 575 "calcparser.g"
						{
#line 575 "calcparser.g"
							  p.step  = reportMomentStep(&_signal); if (_signal) goto _handler;

#line 575 "calcparser.g"
							zzmatch_wsig(TOK_MOMENT_RANGE, _handler);
							
							range = (ANTLRTokenPtr)LT(1);

#line 576 "calcparser.g"
							p.end = mytoken(range)->integerVal();
 consume();
						}
					}
					else {
						if ( (setwd3[LA(1)]&0x10)
 ) {
#line 578 "calcparser.g"
							{
								if ( (LA(1)==TOK_MOMENT_RANGE) ) {
#line 578 "calcparser.g"
									zzmatch_wsig(TOK_MOMENT_RANGE, _handler);
									
									range2 = (ANTLRTokenPtr)LT(1);

#line 579 "calcparser.g"
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
#line 582 "calcparser.g"
				try {
					p.check();
				} catch (com::Exception& msg) {
					genId(start).posError(msg.messages());
				}
			}
		}
		else {
			if ( (LA(1)==TOK_ENDTIME) ) {
#line 589 "calcparser.g"
				zzmatch_wsig(TOK_ENDTIME, _handler);
				
#line 589 "calcparser.g"
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
	resynch(setwd3, 0x20);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

 int  
Parser::reportMomentStep(int *_retsignal)
{
	 int  	 _retv;
#line 593 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr v=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( int  	))
	*_retsignal = NoSignal;
#line 594 "calcparser.g"
	zzmatch_wsig(TOK_PLUS, _handler);
	 consume();
#line 594 "calcparser.g"
	zzmatch_wsig(TOK_INT, _handler);
	
	v = (ANTLRTokenPtr)LT(1);

#line 595 "calcparser.g"
	_retv = mytoken(v)->integerVal();
 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x40);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

  calc::IdList   
Parser::idList(int *_retsignal)
{
	  calc::IdList   	 _retv;
#line 603 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr firstId=NULL, nextId=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof(  calc::IdList   	))
	*_retsignal = NoSignal;
#line 605 "calcparser.g"
	zzmatch_wsig(TOK_LP, _handler);
	 consume();
#line 606 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	firstId = (ANTLRTokenPtr)LT(1);

#line 606 "calcparser.g"
	_retv.push_back(genId(firstId));
 consume();
#line 608 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA) ) {
#line 607 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 607 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			nextId = (ANTLRTokenPtr)LT(1);

#line 607 "calcparser.g"
			_retv.push_back(genId(nextId));
 consume();
		}
	}
#line 609 "calcparser.g"
	zzmatch_wsig(TOK_RP, _handler);
	 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd3, 0x80);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNodeList  
Parser::statementList(int *_retsignal)
{
	 Parser::AP_ASTNodeList  	 _retv;
#line 612 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNodeList  	))
	*_retsignal = NoSignal;
#line 613 "calcparser.g"
	AP_ASTNode s;
#line 620 "calcparser.g"
	{
		while ( (setwd4[LA(1)]&0x1) ) {
#line 614 "calcparser.g"
			{
				if ( (LA(1)==TOK_FOREACH)
 ) {
#line 614 "calcparser.g"
					 s  = foreach(&_signal); if (_signal) goto _handler;

				}
				else {
					if ( (LA(1)==TOK_REPEAT) ) {
#line 614 "calcparser.g"
						 s  = repeat(&_signal); if (_signal) goto _handler;

					}
					else {
						if ( (setwd4[LA(1)]&0x2) ) {
#line 614 "calcparser.g"
							 s  = statement(&_signal); if (_signal) goto _handler;

						}
						else {
							if (_sva) _signal=NoViableAlt;
							else _signal=NoSemViableAlt;
							goto _handler;  /* MR7 */
						}
					}
				}
			}
#line 615 "calcparser.g"
			
			if (!_retv.get())
			_retv.reset(new calc::ASTNodeList());
			_retv->transferPushBack(s.release());
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd4, 0x4);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTStat  
Parser::statement(int *_retsignal)
{
	 Parser::AP_ASTStat  	 _retv;
#line 623 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr ni=NULL, rv=NULL, objectName=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTStat  	))
	*_retsignal = NoSignal;
#line 624 "calcparser.g"
	_retv.reset(new calc::ASTStat());
#line 655 "calcparser.g"
	{
		if ( (setwd4[LA(1)]&0x8) ) {
#line 626 "calcparser.g"
			{
				if ( (LA(1)==TOK_FILEOUTPUT) ) {
#line 626 "calcparser.g"
					zzmatch_wsig(TOK_FILEOUTPUT, _handler);
					
					ni = (ANTLRTokenPtr)LT(1);
 consume();
#line 626 "calcparser.g"
					 AP_ASTNode r_expr  = expr(&_signal); if (_signal) goto _handler;

#line 627 "calcparser.g"
					notImplemented(ni);
					_retv->transferStat(r_expr.release());
				}
				else {
					if ( (setwd4[LA(1)]&0x10)
 ) {
#line 630 "calcparser.g"
						{
							if ( (LA(1)==TOK_REPORT) ) {
#line 630 "calcparser.g"
								zzmatch_wsig(TOK_REPORT, _handler);
								
#line 631 "calcparser.g"
								_retv->setReportParsed(true);
 consume();
#line 633 "calcparser.g"
								{
									if ( (LA(1)==TOK_LP) ) {
#line 633 "calcparser.g"
										zzmatch_wsig(TOK_LP, _handler);
										 consume();
#line 635 "calcparser.g"
										{
											if ( (LA(1)==TOK_ID) ) {
#line 635 "calcparser.g"
												zzmatch_wsig(TOK_ID, _handler);
												
												rv = (ANTLRTokenPtr)LT(1);

#line 635 "calcparser.g"
												_retv->setReportById(genId(rv));
 consume();
											}
											else {
												if ( (setwd4[LA(1)]&0x20) ) {
#line 636 "calcparser.g"
													std::vector<calc::ParsReportMoment> inSitu;
													calc::Id pos(genId(LT(1)));
#line 639 "calcparser.g"
													reportMoments(&_signal, inSitu ); if (_signal) goto _handler;
#line 640 "calcparser.g"
													
													char buf[8];
													static int  inSituNr=0; // harmless static
													sprintf(buf,"%d",inSituNr++);
													pos.setName(buf);
													_retv->transferReportInSitu(
													new calc::Report(pos, inSitu));
												}
												else {
													if (_sva) _signal=NoViableAlt;
													else _signal=NoSemViableAlt;
													goto _handler;  /* MR7 */
												}
											}
										}
#line 649 "calcparser.g"
										zzmatch_wsig(TOK_RP, _handler);
										 consume();
									}
								}
							}
						}
#line 651 "calcparser.g"
						 AP_ASTAss a  = assignment(&_signal); if (_signal) goto _handler;

#line 652 "calcparser.g"
						
						_retv->transferStat(a.release());
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
			if ( (LA(1)==TOK_OBJECT)
 ) {
#line 655 "calcparser.g"
				{
#line 655 "calcparser.g"
					zzmatch_wsig(TOK_OBJECT, _handler);
					 consume();
#line 655 "calcparser.g"
					zzmatch_wsig(TOK_ID, _handler);
					
					objectName = (ANTLRTokenPtr)LT(1);
 consume();
#line 655 "calcparser.g"
					zzmatch_wsig(TOK_IS, _handler);
					 consume();
#line 655 "calcparser.g"
					 AP_ASTNode r_expr  = expr(&_signal); if (_signal) goto _handler;

#line 656 "calcparser.g"
					
					// rewrite to single expr, no assignment
					LinkInExpr *lie=dynamic_cast<LinkInExpr *>(r_expr.get());
					if (!lie)
					r_expr->posError("libraryName::ClassName construct expected");
					lie->setAsConstructor(ASTPar(genId(objectName)));
					_retv->transferStat(r_expr.release());
				}
			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
#line 666 "calcparser.g"
	statementSep(&_signal); if (_signal) goto _handler;
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd4, 0x40);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTStat  
Parser::foreach(int *_retsignal)
{
	 Parser::AP_ASTStat  	 _retv;
#line 669 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr posF=NULL, iterId=NULL, inId=NULL, exceptId=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTStat  	))
	*_retsignal = NoSignal;
#line 670 "calcparser.g"
	// calc::ForEach *f=0;
	calc::Id iter;
	AP_ASTNodeList forEachBody;
	calc::IdList in,except,s;
	bool ascending=true;
#line 676 "calcparser.g"
	zzmatch_wsig(TOK_FOREACH, _handler);
	
	posF = (ANTLRTokenPtr)LT(1);
 consume();
#line 677 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	iterId = (ANTLRTokenPtr)LT(1);
 consume();
#line 678 "calcparser.g"
	zzmatch_wsig(TOK_IN, _handler);
	 consume();
#line 678 "calcparser.g"
	{
		if ( (LA(1)==TOK_ID) ) {
#line 678 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			inId = (ANTLRTokenPtr)LT(1);

#line 678 "calcparser.g"
			in.push_back(genId(inId));
 consume();
		}
		else {
			if ( (LA(1)==TOK_LP) ) {
#line 679 "calcparser.g"
				 in  = idList(&_signal); if (_signal) goto _handler;

			}
			else {
				if (_sva) _signal=NoViableAlt;
				else _signal=NoSemViableAlt;
				goto _handler;  /* MR7 */
			}
		}
	}
#line 681 "calcparser.g"
	{
		if ( (LA(1)==TOK_EXCEPT) ) {
#line 681 "calcparser.g"
			zzmatch_wsig(TOK_EXCEPT, _handler);
			 consume();
#line 682 "calcparser.g"
			{
				if ( (LA(1)==TOK_ID) ) {
#line 682 "calcparser.g"
					zzmatch_wsig(TOK_ID, _handler);
					
					exceptId = (ANTLRTokenPtr)LT(1);

#line 682 "calcparser.g"
					except.push_back(genId(exceptId));
 consume();
				}
				else {
					if ( (LA(1)==TOK_LP)
 ) {
#line 683 "calcparser.g"
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
#line 686 "calcparser.g"
	zzmatch_wsig(TOK_LC, _handler);
	
#line 687 "calcparser.g"
	// f = 0;
	notImplemented(posF);
	//new calc::ForEach(posF.get(),inclIn,
	//genId(iterId),in,except,s,ascending);
	//inclIn->addStatement(f);
 consume();
#line 693 "calcparser.g"
	 forEachBody  = statementList(&_signal); if (_signal) goto _handler;

#line 694 "calcparser.g"
	zzmatch_wsig(TOK_RC, _handler);
	 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd4, 0x80);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::repeat(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 697 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr posR=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 698 "calcparser.g"
	RepeatUntil *r(0);
	AP_ASTNode condition;
	AP_ASTNodeList repeatBody;
#line 702 "calcparser.g"
	zzmatch_wsig(TOK_REPEAT, _handler);
	
	posR = (ANTLRTokenPtr)LT(1);
 consume();
#line 703 "calcparser.g"
	zzmatch_wsig(TOK_LC, _handler);
	 consume();
#line 704 "calcparser.g"
	 repeatBody  = statementList(&_signal); if (_signal) goto _handler;

#line 705 "calcparser.g"
	zzmatch_wsig(TOK_RC, _handler);
	 consume();
#line 705 "calcparser.g"
	zzmatch_wsig(TOK_UNTIL, _handler);
	 consume();
#line 705 "calcparser.g"
	 condition  = expr(&_signal); if (_signal) goto _handler;

#line 705 "calcparser.g"
	zzmatch_wsig(TOK_SC, _handler);
	
#line 706 "calcparser.g"
	
	r=new RepeatUntil(position(posR),repeatBody.release());
	_retv.reset(r);
	r->transferCondition(new NonAssExpr(condition.release()));
 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x1);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTAss  
Parser::assignment(int *_retsignal)
{
	 Parser::AP_ASTAss  	 _retv;
#line 717 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTAss  	))
	*_retsignal = NoSignal;
#line 718 "calcparser.g"
	calc::ASTPar par;
	bool         swap=false;
	AP_ASTNode   right;
	_retv.reset(new calc::ASTAss());
#line 723 "calcparser.g"
	 par  = parWithIndeces(&_signal); if (_signal) goto _handler;

#line 724 "calcparser.g"
	_retv->addPar(par);
#line 728 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA) ) {
#line 726 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 726 "calcparser.g"
			 par  = parWithIndeces(&_signal); if (_signal) goto _handler;

#line 727 "calcparser.g"
			_retv->addPar(par);
		}
	}
#line 729 "calcparser.g"
	 right  = assignmentTail(&_signal, par,swap ); if (_signal) goto _handler;

#line 730 "calcparser.g"
	_retv->transferRhs(right.release());
	if (swap)
	_retv->swap01();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x2);
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

 Parser::AP_ASTNode  
Parser::assignmentTail(int *_retsignal, const calc::ASTPar& lhs, bool& swap )
{
	 Parser::AP_ASTNode  	 _retv;
#line 741 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr ass=NULL, fid1=NULL, fid2=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_ASSOP) ) {
#line 742 "calcparser.g"
		{
#line 742 "calcparser.g"
			zzmatch_wsig(TOK_ASSOP, _handler);
			
			ass = (ANTLRTokenPtr)LT(1);
 consume();
#line 742 "calcparser.g"
			 AP_ASTNode right  = expr(&_signal); if (_signal) goto _handler;

#line 743 "calcparser.g"
			
			// e.g. lhs += expr
			const calc::Operator* op = tokenOp(ass);
			AP_ASTExpr e(createExpr(ass,op));
			e->transferArg(new calc::ASTPar(lhs));
			e->transferArg(right.release());
			_retv=e;
		}
	}
	else {
		if ( (LA(1)==TOK_IS) ) {
#line 751 "calcparser.g"
			{
#line 751 "calcparser.g"
				zzmatch_wsig(TOK_IS, _handler);
				 consume();
#line 783 "calcparser.g"
				{
					if ( (LA(1)==TOK_ID) && (LA(2)==TOK_COMMA) ) {
#line 752 "calcparser.g"
						{
#line 752 "calcparser.g"
							zzmatch_wsig(TOK_ID, _handler);
							
							fid1 = (ANTLRTokenPtr)LT(1);
 consume();
#line 752 "calcparser.g"
							zzmatch_wsig(TOK_COMMA, _handler);
							 consume();
#line 752 "calcparser.g"
							zzmatch_wsig(TOK_ID, _handler);
							
							fid2 = (ANTLRTokenPtr)LT(1);

#line 757 "calcparser.g"
							
							calc::Id sym1(genId(fid1));
							const calc::Operator* op1 = expectFunction(sym1);
							calc::Id sym2(genId(fid2));
							const calc::Operator* op2 = expectFunction(sym2);
 consume();
#line 763 "calcparser.g"
							zzmatch_wsig(TOK_LP, _handler);
							 consume();
#line 763 "calcparser.g"
							 AP_ASTNodeVector args  = exprList(&_signal); if (_signal) goto _handler;

#line 763 "calcparser.g"
							zzmatch_wsig(TOK_RP, _handler);
							
#line 764 "calcparser.g"
							
							if (op1->opCode() != otherOneOfMRF(op2->opCode()) ) {
								/* pcrcalc/test11[bc] */
								sym1.position()->throwError("Functions "
								+quote(op1->name())+" and "+quote(op2->name())
								+" can not be combined");
							}
							swap = oneOfMrfIsStackTop(op1->opCode());
							if (swap) {
								std::swap(op1,op2);
								std::swap(sym1,sym2);
							}
							std::auto_ptr<ASTExpr>
							e(new calc::ASTExpr(
							sym1.position(),
							calc::oneOf2Mrf(op1->opCode())));
							e->transferFunctionArgs(args.release());
							_retv=e;
 consume();
						}
					}
					else {
						if ( (setwd5[LA(1)]&0x4) && 
(setwd5[LA(2)]&0x8) ) {
#line 783 "calcparser.g"
							 AP_ASTNode right  = expr(&_signal); if (_signal) goto _handler;

#line 784 "calcparser.g"
							_retv=right;
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
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x10);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

void
Parser::statementSep(int *_retsignal)
{
#line 788 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_SC) ) {
#line 788 "calcparser.g"
		zzmatch_wsig(TOK_SC, _handler);
		 consume();
	}
	else {
		if ( (LA(1)==TOK_EOF) ) {
#line 789 "calcparser.g"
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
	resynch(setwd5, 0x20);
_handler:
	zzdflthandlers(_signal,_retsignal);
	return;
}

void
Parser::endOfInput(int *_retsignal)
{
#line 792 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	*_retsignal = NoSignal;
#line 792 "calcparser.g"
	{
		while ( (LA(1)==TOK_SC) ) {
#line 792 "calcparser.g"
			zzmatch_wsig(TOK_SC, _handler);
			 consume();
		}
	}
#line 792 "calcparser.g"
	zzmatch_wsig(TOK_EOF, _handler);
	 consume();
	return;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x40);
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

 Parser::AP_ASTNode  
Parser::expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 802 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 803 "calcparser.g"
	AP_ASTNode right;
#line 804 "calcparser.g"
	 _retv  = xor_expr(&_signal); if (_signal) goto _handler;

#line 813 "calcparser.g"
	{
		while ( (LA(1)==TOK_OR) ) {
#line 806 "calcparser.g"
			zzmatch_wsig(TOK_OR, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 806 "calcparser.g"
			 right  = xor_expr(&_signal); if (_signal) goto _handler;

#line 807 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,calc::major2op(OP_OR_)));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd5, 0x80);
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

 Parser::AP_ASTNode  
Parser::xor_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 821 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 822 "calcparser.g"
	AP_ASTNode right;
#line 823 "calcparser.g"
	 _retv  = and_expr(&_signal); if (_signal) goto _handler;

#line 832 "calcparser.g"
	{
		while ( (LA(1)==TOK_XOR)
 ) {
#line 825 "calcparser.g"
			zzmatch_wsig(TOK_XOR, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 825 "calcparser.g"
			 right  = and_expr(&_signal); if (_signal) goto _handler;

#line 826 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,calc::major2op(OP_XOR_)));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x1);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::and_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 834 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 835 "calcparser.g"
	AP_ASTNode right;
#line 836 "calcparser.g"
	 _retv  = eq_expr(&_signal); if (_signal) goto _handler;

#line 845 "calcparser.g"
	{
		while ( (LA(1)==TOK_AND) ) {
#line 838 "calcparser.g"
			zzmatch_wsig(TOK_AND, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 838 "calcparser.g"
			 right  = eq_expr(&_signal); if (_signal) goto _handler;

#line 839 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,calc::major2op(OP_AND_)));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x2);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::eq_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 848 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 849 "calcparser.g"
	AP_ASTNode right;
#line 850 "calcparser.g"
	 _retv  = comp_expr(&_signal); if (_signal) goto _handler;

#line 859 "calcparser.g"
	{
		while ( (LA(1)==TOK_EQ) ) {
#line 852 "calcparser.g"
			zzmatch_wsig(TOK_EQ, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 852 "calcparser.g"
			 right  = comp_expr(&_signal); if (_signal) goto _handler;

#line 853 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,tokenOp(opS)));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x4);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::comp_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 861 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 862 "calcparser.g"
	AP_ASTNode right;
#line 863 "calcparser.g"
	 _retv  = add_expr(&_signal); if (_signal) goto _handler;

#line 872 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMP) ) {
#line 865 "calcparser.g"
			zzmatch_wsig(TOK_COMP, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 865 "calcparser.g"
			 right  = add_expr(&_signal); if (_signal) goto _handler;

#line 866 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,tokenOp(opS)));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x8);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::add_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 874 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 875 "calcparser.g"
	AP_ASTNode right;
#line 877 "calcparser.g"
	 _retv  = mult_expr(&_signal); if (_signal) goto _handler;

#line 888 "calcparser.g"
	{
		while ( (setwd6[LA(1)]&0x10) ) {
#line 879 "calcparser.g"
			zzsetmatch_wsig(ADD_GROUP_set, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 879 "calcparser.g"
			 right  = mult_expr(&_signal); if (_signal) goto _handler;

#line 880 "calcparser.g"
			
			const calc::Operator* op = calc::major2op(
			opS->getType() == TOK_PLUS ? OP_BADD:OP_BMIN);
			AP_ASTExpr e(createExpr(opS,op));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x20);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::mult_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 890 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 891 "calcparser.g"
	AP_ASTNode right;
#line 893 "calcparser.g"
	 _retv  = pow_expr(&_signal); if (_signal) goto _handler;

#line 902 "calcparser.g"
	{
		while ( (setwd6[LA(1)]&0x40)
 ) {
#line 895 "calcparser.g"
			zzsetmatch_wsig(MULT_GROUP_set, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 895 "calcparser.g"
			 right  = pow_expr(&_signal); if (_signal) goto _handler;

#line 896 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,tokenOp(opS)));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd6, 0x80);
	return _retv;
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
	return _retv;
}

 Parser::AP_ASTNode  
Parser::pow_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 910 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 911 "calcparser.g"
	AP_ASTNode right;
#line 912 "calcparser.g"
	 _retv  = sign_expr(&_signal); if (_signal) goto _handler;

#line 921 "calcparser.g"
	{
		while ( (LA(1)==TOK_POW) ) {
#line 914 "calcparser.g"
			zzmatch_wsig(TOK_POW, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 914 "calcparser.g"
			 right  = sign_expr(&_signal); if (_signal) goto _handler;

#line 915 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,calc::major2op(OP_POW)));
			e->transferArg(_retv.release());
			e->transferArg(right.release());
			_retv=e;
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd7, 0x1);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNode  
Parser::sign_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 923 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr s=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 924 "calcparser.g"
	std::vector<calc::Id> signs;
#line 928 "calcparser.g"
	{
		while ( (setwd7[LA(1)]&0x2) && (setwd7[LA(2)]&0x4) ) {
#line 927 "calcparser.g"
			zzsetmatch_wsig(ADD_GROUP_set, _handler);
			
			s = (ANTLRTokenPtr)LT(1);

#line 927 "calcparser.g"
			signs.push_back(genId(s));
 consume();
		}
	}
#line 928 "calcparser.g"
	 _retv  = not_expr(&_signal); if (_signal) goto _handler;

#line 929 "calcparser.g"
	
	for(int i = (int)(signs.size()-1);i>=0; i--) {
		const calc::Operator* op = calc::major2op(
		signs[i].name() == "+" ? OP_UADD:OP_UMIN);
		AP_ASTExpr e(createExpr(signs[i].position(),op));
		e->transferArg(_retv.release());
		_retv=e;
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

 Parser::AP_ASTNode  
Parser::not_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 939 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr s=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
#line 940 "calcparser.g"
	std::vector<calc::Id> signs;
#line 943 "calcparser.g"
	{
		while ( (LA(1)==TOK_NOT) ) {
#line 942 "calcparser.g"
			zzmatch_wsig(TOK_NOT, _handler);
			
			s = (ANTLRTokenPtr)LT(1);

#line 942 "calcparser.g"
			signs.push_back(genId(s));
 consume();
		}
	}
#line 943 "calcparser.g"
	 _retv  = misc_expr(&_signal); if (_signal) goto _handler;

#line 944 "calcparser.g"
	
	for(int i = (int)(signs.size()-1);i>=0; i--) {
		AP_ASTExpr e(createExpr(signs[i].position(), calc::major2op(OP_NOT_)));
		e->transferArg(_retv.release());
		_retv=e;
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

 Parser::AP_ASTNode  
Parser::misc_expr(int *_retsignal)
{
	 Parser::AP_ASTNode  	 _retv;
#line 954 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr opS=NULL, convF=NULL, r=NULL, theId=NULL, nameAfter=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNode  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_LP) ) {
#line 955 "calcparser.g"
		zzmatch_wsig(TOK_LP, _handler);
		 consume();
#line 955 "calcparser.g"
		 _retv  = expr(&_signal); if (_signal) goto _handler;

#line 955 "calcparser.g"
		zzmatch_wsig(TOK_RP, _handler);
		 consume();
	}
	else {
		if ( (LA(1)==TOK_IF)
 ) {
#line 956 "calcparser.g"
			MAJOR_CODE c=OP_IFTHEN;
			AP_ASTNode condn,truen,falsen;
#line 959 "calcparser.g"
			zzmatch_wsig(TOK_IF, _handler);
			
			opS = (ANTLRTokenPtr)LT(1);
 consume();
#line 959 "calcparser.g"
			zzmatch_wsig(TOK_LP, _handler);
			 consume();
#line 959 "calcparser.g"
			 condn  = expr(&_signal); if (_signal) goto _handler;

#line 960 "calcparser.g"
			zzsetmatch_wsig(THEN_GROUP_set, _handler);
			 consume();
#line 960 "calcparser.g"
			 truen  = expr(&_signal); if (_signal) goto _handler;

#line 961 "calcparser.g"
			{
				if ( (setwd7[LA(1)]&0x20) ) {
#line 961 "calcparser.g"
					zzsetmatch_wsig(ELSE_GROUP_set, _handler);
					 consume();
#line 961 "calcparser.g"
					 falsen  = expr(&_signal); if (_signal) goto _handler;

#line 961 "calcparser.g"
					c=OP_IFTHENELSE;
				}
			}
#line 962 "calcparser.g"
			zzmatch_wsig(TOK_RP, _handler);
			
#line 963 "calcparser.g"
			
			AP_ASTExpr e(createExpr(opS,calc::major2op(c)));
			e->transferArg(condn.release());
			e->transferArg(truen.release());
			if (c==OP_IFTHENELSE)
			e->transferArg(falsen.release());
			_retv=e;
 consume();
		}
		else {
			if ( (LA(1)==TOK_CONV_F) ) {
#line 971 "calcparser.g"
				zzmatch_wsig(TOK_CONV_F, _handler);
				
				convF = (ANTLRTokenPtr)LT(1);
 consume();
#line 971 "calcparser.g"
				zzmatch_wsig(TOK_LP, _handler);
				 consume();
#line 973 "calcparser.g"
				{
#line 973 "calcparser.g"
					calc::Id nr;
					if ( (setwd7[LA(1)]&0x40) && (setwd7[LA(2)]&0x80) ) {
#line 974 "calcparser.g"
						 nr  = number(&_signal); if (_signal) goto _handler;

#line 974 "calcparser.g"
						zzmatch_wsig(TOK_RP, _handler);
						
#line 975 "calcparser.g"
						_retv.reset(createCastedASTNumber(convF,nr));
 consume();
					}
					else {
						if ( (setwd8[LA(1)]&0x1) && (setwd8[LA(2)]&0x2) ) {
#line 977 "calcparser.g"
							AP_ASTNode a;
#line 978 "calcparser.g"
							 a  = expr(&_signal); if (_signal) goto _handler;

#line 978 "calcparser.g"
							zzmatch_wsig(TOK_RP, _handler);
							
#line 979 "calcparser.g"
							
							AP_ASTExpr e(createExpr(convF,tokenOp(convF)));
							e->transferArg(a.release());
							_retv=e;
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
				if ( (setwd8[LA(1)]&0x4)
 ) {
#line 985 "calcparser.g"
					calc::Id nr;
#line 986 "calcparser.g"
					 nr  = number(&_signal); if (_signal) goto _handler;

#line 987 "calcparser.g"
					_retv.reset(new calc::ASTNumber(nr));
				}
				else {
					if ( (LA(1)==TOK_REFERENCE) ) {
#line 989 "calcparser.g"
						zzmatch_wsig(TOK_REFERENCE, _handler);
						
						r = (ANTLRTokenPtr)LT(1);

#line 990 "calcparser.g"
						
						_retv.reset(new calc::ASTPar(genId(r)));
 consume();
					}
					else {
						if ( (LA(1)==TOK_ID) ) {
#line 993 "calcparser.g"
							zzmatch_wsig(TOK_ID, _handler);
							
							theId = (ANTLRTokenPtr)LT(1);

#line 994 "calcparser.g"
							
							// empty node list by default
							AP_ASTNodeVector args(new calc::ASTNodeVector());
 consume();
#line 999 "calcparser.g"
							{
								if ( (LA(1)==TOK_LP) ) {
#line 999 "calcparser.g"
									zzmatch_wsig(TOK_LP, _handler);
									
#line 1000 "calcparser.g"
									
									const calc::Operator *o=expectFunction(genId(theId));
 consume();
#line 1003 "calcparser.g"
									{
										if ( (setwd8[LA(1)]&0x8) ) {
#line 1003 "calcparser.g"
											 args  = exprList(&_signal); if (_signal) goto _handler;

										}
									}
#line 1004 "calcparser.g"
									zzmatch_wsig(TOK_RP, _handler);
									
#line 1005 "calcparser.g"
									
									AP_ASTExpr e(createExpr(theId,o));
									e->transferFunctionArgs(args.release());
									_retv=e;
 consume();
								}
								else {
									if ( (LA(1)==TOK_2COL)
 ) {
#line 1010 "calcparser.g"
										zzmatch_wsig(TOK_2COL, _handler);
										 consume();
#line 1011 "calcparser.g"
										zzmatch_wsig(TOK_ID, _handler);
										
										nameAfter = (ANTLRTokenPtr)LT(1);

#line 1012 "calcparser.g"
										
										calc::Id nameBefore = genId(theId);
										std::string strArg;
 consume();
#line 1016 "calcparser.g"
										 args  = modelLinkArgs(&_signal, strArg ); if (_signal) goto _handler;

#line 1017 "calcparser.g"
										
										std::auto_ptr<LinkInExpr> e(new LinkInExpr(nameBefore,genId(nameAfter),strArg));
										// Hack a method with no args, yields a 0-ptr!
										if (!args.get())
										args.reset(new calc::ASTNodeVector());
										e->transferFunctionArgs(args.release());
										_retv=AP_ASTNode(e.release());
									}
									else {
										if ( (setwd8[LA(1)]&0x10) ) {
#line 1026 "calcparser.g"
											
											calc::ASTPar p(genId(theId));
#line 1029 "calcparser.g"
											{
												while ( (LA(1)==TOK_LB) ) {
#line 1029 "calcparser.g"
													 calc::Id a  = arrayIndex(&_signal); if (_signal) goto _handler;

#line 1029 "calcparser.g"
													p.pushBackIndex(a);
												}
											}
#line 1030 "calcparser.g"
											
											_retv.reset(new calc::ASTPar(p));
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
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd8, 0x20);
	return _retv;
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
	return _retv;
}

 Parser::AP_ASTNodeVector  
Parser::modelLinkArgs(int *_retsignal, std::string& strArg )
{
	 Parser::AP_ASTNodeVector  	 _retv;
#line 1041 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr ref=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNodeVector  	))
	*_retsignal = NoSignal;
#line 1042 "calcparser.g"
	zzmatch_wsig(TOK_LP, _handler);
	 consume();
#line 1043 "calcparser.g"
	{
		if ( (LA(1)==TOK_REFERENCE) && (setwd8[LA(2)]&0x40) ) {
#line 1043 "calcparser.g"
			zzmatch_wsig(TOK_REFERENCE, _handler);
			
			ref = (ANTLRTokenPtr)LT(1);

#line 1043 "calcparser.g"
			strArg = genId(ref).name();
 consume();
#line 1044 "calcparser.g"
			{
				if ( (LA(1)==TOK_COMMA) ) {
#line 1044 "calcparser.g"
					zzmatch_wsig(TOK_COMMA, _handler);
					 consume();
				}
			}
		}
	}
#line 1046 "calcparser.g"
	{
		if ( (setwd8[LA(1)]&0x80)
 ) {
#line 1046 "calcparser.g"
			 _retv  = exprList(&_signal); if (_signal) goto _handler;

		}
	}
#line 1047 "calcparser.g"
	zzmatch_wsig(TOK_RP, _handler);
	 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd9, 0x1);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTNodeVector  
Parser::exprList(int *_retsignal)
{
	 Parser::AP_ASTNodeVector  	 _retv;
#line 1051 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTNodeVector  	))
	*_retsignal = NoSignal;
#line 1052 "calcparser.g"
	
	_retv.reset(new calc::ASTNodeVector());
	AP_ASTNode e;
#line 1056 "calcparser.g"
	 e  = expr(&_signal); if (_signal) goto _handler;

#line 1056 "calcparser.g"
	_retv->transferPushBack(e.release());
#line 1058 "calcparser.g"
	{
		while ( (LA(1)==TOK_COMMA) ) {
#line 1057 "calcparser.g"
			zzmatch_wsig(TOK_COMMA, _handler);
			 consume();
#line 1057 "calcparser.g"
			 e  = expr(&_signal); if (_signal) goto _handler;

#line 1057 "calcparser.g"
			_retv->transferPushBack(e.release());
		}
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd9, 0x2);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::ASTPar  
Parser::parWithIndeces(int *_retsignal)
{
	 calc::ASTPar  	 _retv;
#line 1064 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr i=NULL, r=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::ASTPar  	))
	*_retsignal = NoSignal;
#line 1065 "calcparser.g"
	calc::Id a;
#line 1067 "calcparser.g"
	{
		if ( (LA(1)==TOK_ID) ) {
#line 1067 "calcparser.g"
			zzmatch_wsig(TOK_ID, _handler);
			
			i = (ANTLRTokenPtr)LT(1);

#line 1067 "calcparser.g"
			_retv = calc::ASTPar(genId(i));
 consume();
#line 1069 "calcparser.g"
			{
				while ( (LA(1)==TOK_LB) ) {
#line 1068 "calcparser.g"
					 a  = arrayIndex(&_signal); if (_signal) goto _handler;

#line 1068 "calcparser.g"
					_retv.pushBackIndex(a);
				}
			}
		}
		else {
			if ( (LA(1)==TOK_REFERENCE) ) {
#line 1070 "calcparser.g"
				zzmatch_wsig(TOK_REFERENCE, _handler);
				
				r = (ANTLRTokenPtr)LT(1);

#line 1070 "calcparser.g"
				_retv = calc::ASTPar(genId(r));
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
	resynch(setwd9, 0x4);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Id  
Parser::arrayIndex(int *_retsignal)
{
	 calc::Id  	 _retv;
#line 1074 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr id=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Id  	))
	*_retsignal = NoSignal;
#line 1075 "calcparser.g"
	zzmatch_wsig(TOK_LB, _handler);
	 consume();
#line 1075 "calcparser.g"
	zzmatch_wsig(TOK_ID, _handler);
	
	id = (ANTLRTokenPtr)LT(1);

#line 1075 "calcparser.g"
	_retv = genId(id);
 consume();
#line 1075 "calcparser.g"
	zzmatch_wsig(TOK_RB, _handler);
	 consume();
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd9, 0x8);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Id  
Parser::qid(int *_retsignal)
{
	 calc::Id  	 _retv;
#line 1079 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr i=NULL, r=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Id  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_ID)
 ) {
#line 1079 "calcparser.g"
		zzmatch_wsig(TOK_ID, _handler);
		
		i = (ANTLRTokenPtr)LT(1);

#line 1079 "calcparser.g"
		_retv = genId(i);
 consume();
	}
	else {
		if ( (LA(1)==TOK_REFERENCE) ) {
#line 1080 "calcparser.g"
			zzmatch_wsig(TOK_REFERENCE, _handler);
			
			r = (ANTLRTokenPtr)LT(1);

#line 1080 "calcparser.g"
			_retv = genId(r);
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
	resynch(setwd9, 0x10);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Id  
Parser::number(int *_retsignal)
{
	 calc::Id  	 _retv;
#line 1086 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Id  	))
	*_retsignal = NoSignal;
#line 1087 "calcparser.g"
	calc::Id signS,valS;
#line 1088 "calcparser.g"
	{
		if ( (setwd9[LA(1)]&0x20) ) {
#line 1088 "calcparser.g"
			 signS  = sign(&_signal); if (_signal) goto _handler;

		}
	}
#line 1090 "calcparser.g"
	 valS  = unsignedNumber(&_signal); if (_signal) goto _handler;

#line 1091 "calcparser.g"
	
	if (signS.name().empty())
	_retv = valS;
	else {
		// TODO er zouden spaties tussen signS en valS kunnen zitten!
		signS.setName(signS.name()+valS.name());
		_retv = signS;
	}
	return _retv;
fail:
	syn(zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk);
	resynch(setwd9, 0x40);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Id  
Parser::unsignedNumber(int *_retsignal)
{
	 calc::Id  	 _retv;
#line 1102 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr v1=NULL, v2=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Id  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_INT) ) {
#line 1103 "calcparser.g"
		zzmatch_wsig(TOK_INT, _handler);
		
		v1 = (ANTLRTokenPtr)LT(1);

#line 1104 "calcparser.g"
		_retv = genId(v1);
 consume();
	}
	else {
		if ( (LA(1)==TOK_FLOAT) ) {
#line 1105 "calcparser.g"
			zzmatch_wsig(TOK_FLOAT, _handler);
			
			v2 = (ANTLRTokenPtr)LT(1);

#line 1106 "calcparser.g"
			_retv = genId(v2);
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
	resynch(setwd9, 0x80);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Id  
Parser::sign(int *_retsignal)
{
	 calc::Id  	 _retv;
#line 1109 "calcparser.g"
	zzRULE;
	int _sva=1;
	ANTLRTokenPtr v1=NULL, v2=NULL;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Id  	))
	*_retsignal = NoSignal;
	if ( (LA(1)==TOK_PLUS)
 ) {
#line 1110 "calcparser.g"
		zzmatch_wsig(TOK_PLUS, _handler);
		
		v1 = (ANTLRTokenPtr)LT(1);

#line 1111 "calcparser.g"
		_retv =genId(v1);
 consume();
	}
	else {
		if ( (LA(1)==TOK_MINUS) ) {
#line 1112 "calcparser.g"
			zzmatch_wsig(TOK_MINUS, _handler);
			
			v2 = (ANTLRTokenPtr)LT(1);

#line 1113 "calcparser.g"
			_retv =genId(v2);
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
	resynch(setwd10, 0x1);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 Parser::AP_ASTId  
Parser::nrOrPar(int *_retsignal)
{
	 Parser::AP_ASTId  	 _retv;
#line 1119 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( Parser::AP_ASTId  	))
	*_retsignal = NoSignal;
#line 1120 "calcparser.g"
	calc::Id i;
	if ( (setwd10[LA(1)]&0x2) ) {
#line 1121 "calcparser.g"
		 i  = number(&_signal); if (_signal) goto _handler;

#line 1121 "calcparser.g"
		_retv.reset(new ASTNumber(i));
	}
	else {
		if ( (setwd10[LA(1)]&0x4) ) {
#line 1122 "calcparser.g"
			 i  = qid(&_signal); if (_signal) goto _handler;

#line 1122 "calcparser.g"
			_retv.reset(new ASTPar(i));
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
	resynch(setwd10, 0x8);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}

 calc::Id  
Parser::nrOrId(int *_retsignal)
{
	 calc::Id  	 _retv;
#line 1127 "calcparser.g"
	zzRULE;
	int _sva=1;
	int _signal=NoSignal;
	PURIFY(_retv,sizeof( calc::Id  	))
	*_retsignal = NoSignal;
#line 1128 "calcparser.g"
	calc::Id i;
	if ( (setwd10[LA(1)]&0x10) ) {
#line 1129 "calcparser.g"
		 i  = number(&_signal); if (_signal) goto _handler;

#line 1129 "calcparser.g"
		_retv=i;
	}
	else {
		if ( (setwd10[LA(1)]&0x20)
 ) {
#line 1130 "calcparser.g"
			 i  = qid(&_signal); if (_signal) goto _handler;

#line 1130 "calcparser.g"
			_retv=i;
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
	resynch(setwd10, 0x40);
	return _retv;
_handler:
	zzdflthandlers(_signal,_retsignal);
	return _retv;
}
