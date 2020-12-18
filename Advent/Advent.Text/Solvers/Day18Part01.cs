using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("18.1")]
    public class Day18Part01 : Solver
    { 

        public override object Solve()
        {
            var problems = ReadInputFile();

            var result = problems.Trim()
                .Split('\n')
                .Select(AdventMath.Arithmetic.EvaluateExpression)
                .Sum();

            return result;
        }

    }
}
