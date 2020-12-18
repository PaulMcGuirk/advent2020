using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.AdventMath
{
    public static class Arithmetic
    {
        /// <summary>
        /// Evaluate the expression treating addition and multiplication
        /// as having equal precedence.
        /// </summary>
        /// <param name="s">The expression to evaluate</param>
        /// <returns>The value</returns>
        public static long EvaluateExpression(string s)
        {
            s = s.Replace("(", "( ").Replace(")", " )");
            var pieces = new List<string>(s.Split(' ')).GetEnumerator();

            return EvaluateExpression(pieces);
        }

        /// <summary>
        /// Evaluate the expression treating addition and multiplication
        /// as having equal precedence.
        /// </summary>
        /// <param name="pieces">The pieces of the expression</param>
        /// <returns>The value</returns>
        private static long EvaluateExpression(IEnumerator<string> pieces)
        { 
            pieces.MoveNext();

            var value = pieces.Current == "(" ? EvaluateExpression(pieces) : int.Parse(pieces.Current);

            while (pieces.MoveNext())
            {
                if (pieces.Current == ")")
                {
                    break;
                }

                var op = pieces.Current;

                pieces.MoveNext();
                var next = pieces.Current == "(" ? EvaluateExpression(pieces) : int.Parse(pieces.Current);

                value = op switch
                {
                    "+" => value + next,
                    "*" => value * next,
                    _ => throw new InvalidOperationException($"{op}")
                };
            }

            return value;   
        }

        private static readonly string[] Operators = new string[] { "+", "*" };
        /// <summary>
        /// Evaluate the expression assumping that addition has higher
        /// precendence than multiplication
        /// </summary>
        /// <param name="s">The expression to evaluate</param>
        /// <returns>The value</returns>
        public static long EvaluateExpressionAdvanced(string s)
        {
            s = s.Replace("(", "( ").Replace(")", " )");
            var pieces = new List<string>(s.Split(' '));

            return EvaluateExpressionAdvanced(pieces);
        }

        /// <summary>
        /// Evaluate the expression assumping that addition has higher
        /// precendence than multiplication
        /// </summary>
        /// <param name="pieces">The pieces of the expression to evaluate</param>
        /// <returns>The value</returns>
        private static long EvaluateExpressionAdvanced(List<string> pieces)
        {
            EvaluateSubExpressionsAdvanced(pieces);
            EvaluateOperatorsAdvanced(pieces);

            return long.Parse(pieces[0]);
        }

        /// <summary>
        /// Evaluate sub expressions using the advanced precendence.
        /// Sub expressions will be removed and replaced with their values
        /// </summary>
        /// <param name="pieces">The expression pieces</param>
        private static void EvaluateSubExpressionsAdvanced(List<string> pieces)
        {
            // evaluate sub expressions
            while (true)
            {
                var leftPos = pieces.IndexOf("(");
                if (leftPos < 0)
                {
                    break;
                }

                var rightPos = leftPos + 1;
                var stackLevel = 1;
                for (; stackLevel > 0; rightPos++)
                {
                    if (pieces[rightPos] == "(")
                    {
                        stackLevel++;
                    }
                    else if (pieces[rightPos] == ")")
                    {
                        stackLevel--;
                    }
                }
                rightPos--;

                var subPieces = pieces.GetRange(leftPos + 1, rightPos - leftPos - 1);
                var value = EvaluateExpressionAdvanced(subPieces);

                // replace the subexpression with its value
                pieces[leftPos] = value.ToString();
                pieces.RemoveRange(leftPos + 1, rightPos - leftPos);
            }
        }

        /// <summary>
        /// Evaluate all operators, assuming that all subexpressions have been resolved
        /// </summary>
        /// <param name="pieces">The pieces to evaluate</param>
        private static void EvaluateOperatorsAdvanced(List<string> pieces)
        { 
            // evaluate operators
            foreach (var op in Operators)
            {
                while (true)
                {
                    var pos = pieces.IndexOf(op);
                    if (pos == -1)
                    {
                        break;
                    }

                    var left = long.Parse(pieces[pos - 1]);
                    var right = long.Parse(pieces[pos + 1]);

                    var val = op switch
                    {
                        "+" => left + right,
                        "*" => left * right,
                        _ => throw new InvalidOperationException($"{op}")
                    };

                    pieces[pos - 1] = val.ToString();
                    pieces.RemoveRange(pos, 2);
                }
            }
        }
    }
}
