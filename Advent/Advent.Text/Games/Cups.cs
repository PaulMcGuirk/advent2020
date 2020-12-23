using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Games
{
    /// <summary>
    /// A game of Crab Cups
    /// </summary>
    public class Cups
    {
        private readonly int[] _next;
        private readonly int _pickupCount;
        private readonly int[] _removed; // temporarily stores cups that have been removed
        private readonly int _maxValue;

        private int _current;

        /// <summary>
        /// Create a new game of cups with the given collection of entries
        /// </summary>
        /// <param name="nums"></param>
        /// <param name="pickupCount">The number of cups to pick up during the
        /// pickup segment </param>
        /// <param name="fillTo">If set, additional cups will be added until
        /// the total number of cups is equal to this parameter</param>
        public Cups(IEnumerable<int> nums, int pickupCount, int? fillTo = null)
        {
            var maxInNums = nums.Max();

            _maxValue = fillTo ?? maxInNums;

            _next = new int[_maxValue + 1];
            
            var first = nums.First();
            _current = first;

            foreach (var num in nums.Skip(1))
            {
                // set the numbers passed in
                _next[_current] = num;
                _current = num;
            }

            if (fillTo.HasValue)
            {
                // do the filling
                _next[_current] = maxInNums + 1;
                _current = maxInNums + 1;

                while (_current < _maxValue)
                {
                    _next[_current] = ++_current;
                }
            }

            // close the loop and set the current pointer
            _next[_current] = first;
            _current = first;

            _pickupCount = pickupCount;
            _removed = new int[_pickupCount];
        }

        /// <summary>
        /// Play a number of rounds
        /// </summary>
        /// <param name="numRounds">The number of rounds to play</param>
        public void Play(int numRounds)
        {
            for (var i = 0; i < numRounds; i++)
            {
                PlayRound();
            }
        }

        /// <summary>
        /// Play a single round of Crab Cups
        /// </summary>
        public void PlayRound()
        {
            RemoveCups();

            var destination = _current - 1;
            while (destination <= 0 || _removed.Contains(destination))
            {
                destination--;
                if (destination <= 0)
                {
                    destination = _maxValue;
                }
            }

            ReplaceCups(destination);

            _current = _next[_current];
        }

        /// <summary>
        /// Get all of the cups, starting with the input
        /// </summary>
        /// <param name="startingCup">The value of the first cup to get</param>
        /// <returns>All of the cups</returns>
        public IEnumerable<int> GetCups(int startingCup)
        {
            yield return startingCup;
            for (var val = _next[startingCup]; val != startingCup; val = _next[val])
            {
                yield return val;
            }
        }

        /// <summary>
        /// Perform the remove step, updating list pointers
        /// to skip over the removed, and storing the removed values
        /// in the removed list
        /// </summary>
        private void RemoveCups()
        {
            var pos = _current;
            for (var i = 0; i < _pickupCount; i++)
            {
                pos = _next[pos];
                _removed[i] = pos;
            }

            _next[_current] = _next[pos];
        }

        /// <summary>
        /// Replace the removed cups by updating pointers.
        /// </summary>
        /// <param name="cup">The cup to start placing after</param>
        private void ReplaceCups(int cup)
        {
            var next = _next[cup];
            _next[cup] = _removed[0];
            _next[_removed[_pickupCount - 1]] = next;
        }
    }
}
