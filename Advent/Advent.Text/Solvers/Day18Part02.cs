using System.Linq;

namespace Advent.Text.Solvers
{
    [Solver("18.2")]
    public class Day18Part02 : Solver
    { 

        public override object Solve()
        {
            var problems = ReadInputFile();

            var result = problems.Trim()
                .Split('\n')
                .Select(AdventMath.Arithmetic.EvaluateExpressionAdvanced)
                .Sum();

            return result;
        }

    }
}
