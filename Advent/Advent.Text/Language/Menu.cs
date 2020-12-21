using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Language
{
    public class Menu
    {
        public List<MenuItem> Items { get;  }

        private List<string> _safeIngredients;
        /// <summary>
        /// A list of the safe ingredients on this menu.
        /// </summary>
        public List<string> SafeIngredients
        {
            get
            {
                if (_safeIngredients == null)
                {
                    Calculate();
                }
                return _safeIngredients;
            }
        }

        private Dictionary<string, string> _unsafeIngredients;
        /// <summary>
        /// The unsafe ingredients on this menu. The keys are ingredient
        /// names while the values are the allergens.
        /// </summary>
        public Dictionary<string, string> UnsafeIngredients
        {
            get
            {
                if (_unsafeIngredients == null)
                {
                    Calculate();
                }
                return _unsafeIngredients;
            }
        }

        public Menu(string contents)
        {
            Items = contents.Trim().Split('\n')
                .Select(i => new MenuItem(i)).ToList();
        }

        public override string ToString() => string.Join("\n", Items);

        /// <summary>
        /// Determine which menu items contain which ingredients
        /// </summary>
        private void Calculate()
        {
            var allIngredients = Items.SelectMany(item => item.Ingredients).ToHashSet();
            var possibleIngredients = Items.SelectMany(item => item.ListedAllergens)
                .Distinct()
                .ToDictionary(allergen => allergen, allergen => new HashSet<string>(allIngredients));

            foreach (var item in Items)
            {
                foreach (var allergen in item.ListedAllergens)
                {
                    possibleIngredients[allergen].IntersectWith(item.Ingredients);
                }
            }

            _safeIngredients = allIngredients
                .Where(ingredient => !possibleIngredients.Values.Any(x => x.Contains(ingredient)))
                .ToList();

            while (possibleIngredients.Values.Any(x => x.Count > 1))
            {
                // this might be an infinite loop in general. Worry about that if it happens
                foreach (var (allergen, ingredients) in possibleIngredients)
                {
                    if (ingredients.Count > 1)
                    {
                        continue;
                    }

                    var ingredient = ingredients.First();
                    foreach (var (otherAllergen, otherIngredients) in possibleIngredients)
                    {
                        if (otherAllergen == allergen)
                        {
                            continue;
                        }
                        otherIngredients.Remove(ingredient);
                    }
                }
            }

            _unsafeIngredients = possibleIngredients
                .ToDictionary(pair => pair.Key, pair => pair.Value.Single());
        }
    }

    public class MenuItem
    {
        /// <summary>
        /// The ingredients listed for this menu item
        /// </summary>
        public List<string> Ingredients { get; }
        /// <summary>
        /// The allergens listed in the menu. This will not necessarily be the
        /// complete list of allergens
        /// </summary>
        public List<string> ListedAllergens { get; }

        public MenuItem(string s)
        {
            var pieces = s.Trim().Split("(contains");

            Ingredients = pieces[0].Trim()
                .Split(' ')
                .Select(ingredient => ingredient.Trim())
                .ToList();

            ListedAllergens = pieces[1].Trim().Replace(")", "")
                .Split(',')
                .Select(allergen => allergen.Trim())
                .ToList();
        }

        public override string ToString()
            => $"Ingredients: {string.Join(", ", Ingredients)}\nListed Allergens: {string.Join(",", ListedAllergens)}";

    }
}
