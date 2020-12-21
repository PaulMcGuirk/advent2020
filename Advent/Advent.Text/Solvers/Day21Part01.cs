using System.Linq;
using Advent.Text.Language;

namespace Advent.Text.Solvers
{
    [Solver("21.1")]
    public class Day21Part01 : Solver
    { 
        public override object Solve()
        {
            var contents = ReadInputFile();

            var menu = new Menu(contents);
            var safeIngredients = menu.SafeIngredients;

            var result = menu.Items.Select(item => safeIngredients.Count(ingredient => item.Ingredients.Contains(ingredient))).Sum();

            return result;
        }

    }
}
