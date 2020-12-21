using System;
using System.Linq;
using Advent.Text.Language;

namespace Advent.Text.Solvers
{
    [Solver("21.2")]
    public class Day21Part02 : Solver
    { 
        public override object Solve()
        {
            var contents = ReadInputFile();

            var menu = new Menu(contents);
            var unsafeIngredients = menu.UnsafeIngredients;

            var result = string.Join(',', unsafeIngredients
                .OrderBy(pair => pair.Key)
                .Select(pair => pair.Value));

            return result;
        }

    }
}
