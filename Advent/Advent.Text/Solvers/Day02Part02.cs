using System.Linq;
using System.Text.RegularExpressions;

namespace Advent.Text.Solvers
{
    [Solver("2.2")]
    public class Day02Part02 : Solver
    {
        private static Regex _passwordRx = new Regex(@"^(?<posA>\d+)-(?<posB>\d+) (?<reqCh>[a-z]): (?<password>[a-z]+)$");

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
            var posA = int.Parse(match.Groups["posA"].Value);
            var posB = int.Parse(match.Groups["posB"].Value);
            var reqCh = match.Groups["reqCh"].Value[0];
            var password = match.Groups["password"].Value;

            return (password[posA - 1] == reqCh) ^ (password[posB - 1] == reqCh);
        }
    }
}
