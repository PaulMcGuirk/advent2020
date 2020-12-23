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
        private readonly Dictionary<int, int> _next; // the value is the cup that comes after the key
        private int _current;
        
        private readonly int _maxValue;

        /// <summary>
        /// Create a new game of cups with the given collection of entries
        /// </summary>
        /// <param name="nums"></param>
        public Cups(IEnumerable<int> nums)
        {
            _next = new Dictionary<int, int>();

            var first = nums.First();
            _current = first;
            _maxValue = first;

            foreach (var num in nums.Skip(1))
            {
                _next[_current] = num;
                _current = num;
                if (num > _maxValue)
                {
                    _maxValue = num;
                }
            }

            _next[_current] = first;
            _current = first;
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
            var removed = RemoveNext(3);

            var destination = _current - 1;
            while (destination <= 0 || removed.Contains(destination))
            {
                destination--;
                if (destination <= 0)
                {
                    destination = _maxValue;
                }
            }

            AddListAfter(destination, removed);

            //for (var i = 0; i < 3; i++)
            //{
            //    AddAfter(destination, removed[2 - i]);
            //}

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
        /// Remove a number of nodes from the list
        /// </summary>
        /// <param name="numToRemove">The numbers to remove</param>
        /// <returns>The nodes removed</returns>
        private int[] RemoveNext(int numToRemove)
        {
            var removed = new int[numToRemove];

            var pos = _current;
            for (var i = 0; i < numToRemove; i++)
            {
                pos = _next[pos];
                removed[i] = pos;
            }

            _next[_current] = _next[pos];
            // note we're not actually removing anything, just updating
            // pointers to skip

            return removed;
        }

        /// <summary>
        /// Add some values to the given position. It's assumed that
        /// these values are already linked together among themselve
        /// </summary>
        /// <param name="pos">The position to add after</param>
        /// <param name="listVals">The values to add</param>
        private void AddListAfter(int pos, int[] listVals)
        {
            var next = _next[pos];
            _next[pos] = listVals[0];
            _next[listVals[^1]] = next;
        }
    }
}
