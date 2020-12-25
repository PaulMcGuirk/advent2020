using System.Linq;
using Advent.Text.Security;

namespace Advent.Text.Solvers
{
    [Solver("25.1")]
    public class Day25Part01 : Solver
    {
        public override object Solve()
        {
            var contents = ReadInputFile();

            var publicKeys = contents.Trim().Split('\n').Select(int.Parse).ToArray();

            var result = Keycard.GetEncryptionKey(publicKeys[0], publicKeys[1]);
            
            return result;
        }
    }
}
