package fr.istic.cal.interpreter

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

object Interpreter {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case Nil => NlValue
      case (vm,em) :: t => if(vm == v) em else lookUp(v,t)
    }
  }

  /**
   * @param v : une variable
   * @param d : une valeur
   * @param mem : une mémoire
   * @return la mémoire, augmentée de l'assignation [v->d] si v n'était pas présente dans la mémoire,
   * modifiée pour prendre en compte la nouvelle valeur de v sinon
   */
  def assign(v: Variable, d: Value, mem: Memory): Memory = {
    mem match {
      case Nil => (v,d) :: Nil
      case (vm,em) :: t => if(v == vm) (v,d) :: t else (vm,em) :: assign(v,d,t)
    }
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl => NlValue
      case Cst(name) => mem match {
                          case Nil => NlValue
                          case (_, tete)::queue => if (tete ==  CstValue(name)) return CstValue(name)
                                                   else interpreterExpr(expression, queue)
                        }
      case VarExp(name) => lookUp(Var(name) , mem)
      case Cons(arg1 , arg2) => ConsValue(interpreterExpr(arg1,mem) , interpreterExpr(arg2,mem))
      case Hd(arg) => arg match {
                        case Cons(arg1,arg2) => interpreterExpr(arg1,mem)
                        case VarExp(name) => lookUp(Var(name),mem) match {
                                               case ConsValue(expr1,expr2) => expr1
                                               case _ => NlValue
                                             }
                        case _ => NlValue
                      }
      case Tl(arg) => arg match {
                        case Cons(arg1,arg2) => interpreterExpr(arg2,mem)
                        case VarExp(name) => lookUp(Var(name),mem) match {
                                               case ConsValue(expr1,expr2) => expr2
                                               case _ => NlValue
                                             }
                        case _ => NlValue
                      }
      case Eq(arg1,arg2) => if(interpreterExpr(arg1,mem) == interpreterExpr(arg2,mem)) interpreterExpr(arg1,mem)
                            else NlValue
      
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant une expression de cette valeur
   */
  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue => Nl
      case CstValue(name) => Cst(name)
      case ConsValue(arg1,arg2) => Cons(valueToExpression(arg1) , valueToExpression(arg2))
    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop => memory
      case Set(variable,expr) => if(lookUp(variable,memory) == interpreterExpr(expr,memory)) memory
                                 else assign(variable,interpreterExpr(expr,memory),memory)
      case While(expr,body) => if(interpreterExpr(expr, memory) == NlValue) memory
                                        else {
                                          val memorybis = interpreterCommands(body, memory)
                                          if (interpreterExpr(expr, memorybis) == NlValue) memorybis
                                          else interpreterCommand(While(expr, body), memorybis)
                                        } 
      case For(count,body) => interpreterExpr(count, memory) match {
                                case NlValue => memory
                                case CstValue(name) => interpreterCommands(body, memory)
                                case ConsValue(arg1, arg2) => 
                                  val countbis = valueToExpression(interpreterExpr(Tl(count), memory))
                                  interpreterCommand( For(countbis, body), interpreterCommands(body, memory))
        }
      case If(cond,then_commands,else_commands) => if(interpreterExpr(cond,memory) != NlValue) interpreterCommands(then_commands,memory)
                                                   else interpreterCommands(else_commands,memory)
                               
    }
  }

  /**
   * @param commands : une liste non vide d'AST décrivant une liste non vide de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil => memory
      case commande :: Nil => interpreterCommand(commande,memory)
      case commande :: reste => 
          val memorybis:Memory=interpreterCommand(commande,memory)
          interpreterCommands(reste,memorybis)
    }
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste non vide décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste non vide de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    (vars,vals) match {
      case (Nil,Nil) => Nil
      case (debut_vars :: reste_vars , debut_vals :: reste_vals) => (debut_vars,debut_vals) :: interpreterMemorySet(reste_vars,reste_vals)
      case(_ , _) => Nil // listes de longueurs différentes (scala demande à gérer ce cas)
    }
  }

  /**
   * @param vars : une liste non vide décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    vars match {
      case Nil => Nil
      case debut :: reste =>  lookUp(debut,memory) :: interpreterMemoryGet(reste,memory)
    }
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  // TODO TP2
  def interpreter(program: Program, vals: List[Value]): List[Value] = {
   program match {
     case Progr(in,commands,out) => interpreterMemoryGet(out, interpreterCommands(commands,interpreterMemorySet(in,vals)))
   }
  }
  
  def main(args: Array[String]): Unit = {
    
  }

}