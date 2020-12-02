using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.Solvers
{
    [Solver("2.1")]
    public class Day02Part01 : Solver
    {
        private static Regex _passwordRx = new Regex(@"^(?<min>\d+)-(?<max>\d+) (?<reqCh>[a-z]): (?<password>[a-z]+)$");

        public override object Solve()
        {
            var contents = ReadInputFile();
            return contents.Split('\n')
                .Where(IsValidPasswordDatabaseLine)
                .Count();
        }

        private bool IsValidPasswordDatabaseLine(string line)
        {
            if (string.IsNullOrEmpty(line))
            {
                return false;
            }

            var match = _passwordRx.Match(line);
            var min = int.Parse(match.Groups["min"].Value);
            var max = int.Parse(match.Groups["max"].Value);
            var reqCh = match.Groups["reqCh"].Value[0];
            var password = match.Groups["password"].Value;

            var requiredCharCount = password.Where(ch => ch == reqCh).Count();
            return (min <= requiredCharCount) && (requiredCharCount <= max);
        }
    }
}
