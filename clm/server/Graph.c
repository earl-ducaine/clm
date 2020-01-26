/*
 *   Copyright: VW-GEDAS, Pascalstr. 11, 1000 Berlin 10
 *
 *   Author: Thomas Spandoeck
 *            VW-GEDAS
 *            Pascalstr. 11
 *            1000 Berlin 10
 */

#ifdef GRAPHWIDGET

#include <stdio.h>
#include <sys/types.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xbab/Graph.h>

#include "interface.h"
#include "functions.h"

#ifdef SINGLE_RELATION
ClmCommand *ClmFaddGraphRelation(cmd)
ClmCommand *cmd;
{
  Widget          ancestor, decedent;
  char            *msg;
  char            err_msg[100];

  if(cmd->command != ClmAddGraphRelation || cmd->num_arg != 3)
    GenError("ClmAddGraphRelation: illegal command record");

  if( msg = LookupWidget(cmd->args[0].v.int_value, &ancestor, NULL ,NULL) )
        GenWarn(msg);
  if( msg = LookupWidget(cmd->args[1].v.int_value, &decedent, NULL ,NULL) )
        GenWarn(msg);

  if(!(strcmp(cmd->args[2].v.symbol_value,"SOLID-LINE"))){
    XmAddGraphRelation(ancestor,decedent,XmSOLID_LINE);
    return(NULL);
  }
  if(!(strcmp(cmd->args[2].v.symbol_value,"DASHED-LINE"))){
    XmAddGraphRelation(ancestor,decedent,XmDASHED_LINE);
    return(NULL);
  }
  sprintf(err_msg, "Invalid Relation-Type for Graph-Widget");
  GenWarn(err_msg);
  return(NULL);
}

ClmCommand *ClmFremoveGraphRelation(cmd)
ClmCommand *cmd;
{
  Widget          ancestor, decedent;
  char            *msg;
  char            err_msg[100];

  if(cmd->command != ClmRemoveGraphRelation || cmd->num_arg != 2)
    GenError("ClmRemoveGraphRelation: illegal command record");

  if( msg = LookupWidget(cmd->args[0].v.int_value, &ancestor, NULL ,NULL) )
        GenWarn(msg);
  if( msg = LookupWidget(cmd->args[1].v.int_value, &decedent, NULL ,NULL) )
        GenWarn(msg);

/*  if(!(strcmp(cmd->args[2].v.symbol_value,"SOLID-LINE"))){
    XmRemoveGraphRelation(ancestor,decedent,XmSOLID_LINE);
    return(NULL);
  }
  if(!(strcmp(cmd->args[2].v.symbol_value,"DASHED-LINE"))){
    XmRemoveGraphRelation(ancestor,decedent,XmDASHED_LINE);
    return(NULL);
  }
  sprintf(err_msg, "Invalid Relation-Type for Graph-Widget");
  GenWarn(err_msg);
  return(NULL);
  */
  XmRemoveGraphRelation(ancestor, decedent);
}
#endif

ClmCommand *ClmFdoLayout(cmd)
ClmCommand *cmd;
{
  Widget graph;
  char            *msg;
  char            err_msg[100];

  if(cmd->command != ClmDoLayout || cmd->num_arg != 1)
    GenError("ClmDoLayout: illegal command record");
  
  if( msg = LookupWidget(cmd->args[0].v.int_value, &graph, NULL ,NULL) )
    GenWarn(msg);
  XmDoLayout(graph);
  return(NULL);
}
   
ClmCommand *ClmFaddGraphRelations(cmd)
ClmCommand *cmd;
{
  Widget          ancestor, decedent;
  char            *msg;
  char            err_msg[100];
  WidgetList      pairs =  (WidgetList) XtMalloc(sizeof(Widget) *10);
  int      max = 10;
  int i;

  if(cmd->command != ClmAddGraphRelations || cmd->num_arg < 3)
    GenError("ClmAddGraphRelations: illegal command record");
  for(i=1;i<cmd->num_arg;i+=2){
    if( msg = LookupWidget(cmd->args[i].v.int_value, &pairs[i-1], NULL ,NULL))
      GenWarn(msg);
    if( msg = LookupWidget(cmd->args[i+1].v.int_value, &pairs[i], NULL ,NULL))
      GenWarn(msg);
    if(max <= (cmd->num_arg - 1)){
      pairs = (WidgetList) XtRealloc(pairs, max*2 * sizeof(Widget));
      max *= 2;
    }
  }
  if(!(strcmp(cmd->args[0].v.symbol_value,"SOLID-LINE"))){
    XmAddGraphRelations(pairs,i-1 ,XmSOLID_LINE);
    return(NULL);
  }
  if(!(strcmp(cmd->args[0].v.symbol_value,"DASHED-LINE"))){
    XmAddGraphRelations(pairs,i-1 ,XmDASHED_LINE);
    return(NULL);
  }
  XtFree(pairs);
  sprintf(err_msg, "Invalid Relation-Type for Graph-Widget");
  GenWarn(err_msg);
  return(NULL);
}
ClmCommand *ClmFremoveGraphRelations(cmd)
ClmCommand *cmd;
{
  Widget          ancestor, decedent;
  char            *msg;
  char            err_msg[100];
  WidgetList      pairs =  (WidgetList) XtMalloc(sizeof(Widget) *10);
  int      max = 10;
  int i;

  if(cmd->command != ClmRemoveGraphRelations || cmd->num_arg < 3)
    GenError("ClmRemoveGraphRelation: illegal command record");
  for(i=1;i<cmd->num_arg;i+=2){
    if( msg = LookupWidget(cmd->args[i].v.int_value, &pairs[i-1], NULL ,NULL))
      GenWarn(msg);
    if( msg = LookupWidget(cmd->args[i+1].v.int_value, &pairs[i], NULL ,NULL))
      GenWarn(msg);
    if(max <= (cmd->num_arg - 1)){
      pairs = (WidgetList) XtRealloc(pairs, max*2 * sizeof(Widget));
      max *= 2;
    }
  }

  if(!(strcmp(cmd->args[2].v.symbol_value,"SOLID-LINE"))){
    XmRemoveGraphRelations(pairs,i-1 /* ,XmSOLID_LINE */);
    return(NULL);
  }
  if(!(strcmp(cmd->args[2].v.symbol_value,"DASHED-LINE"))){
    XmRemoveGraphRelations(pairs,i-1 /*,XmDASHED_LINE */);
    return(NULL);
  }
  sprintf(err_msg, "Invalid Relation-Type for Graph-Widget");
  GenWarn(err_msg);
  return(NULL);

}

#else

static int no_graph_widget = 1;

#endif /* GRAPHWIDGET */
