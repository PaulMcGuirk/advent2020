using System;
using System.Collections.Generic;
using System.Linq;

namespace Advent.Text.Life
{
    /// <summary>
    /// A rule for the game of life. Given the counts of neighbors as a dictionary
    /// where the char is the tile type and the int is the number of neighbors
    /// of that type, the delegate should return the next state. The idea
    /// is that each tile type will have its own rule.
    /// </summary>
    /// <param name="neightborCounts">The neighbors</param>
    /// <returns>The next state</returns>
    public delegate char NextStateRule(Dictionary<char, int> neightborCounts);

    public enum NeighborCountingRules
    {
        /// <summary>
        /// A neighbor is the simply an adjacent tile
        /// </summary>
        Adjacent,
        /// <summary>
        /// A neighbor is the closest cell in a given direction that is not
        /// an empty cell
        /// </summary>
        Closest
    }

    /// <summary>
    /// A generic game of 2D life on a finite rectangle with multiple states and
    /// rules that are specified by the consumer
    /// </summary>
    public class GenericLife
    {
        /// <summary>
        /// The allowed tile states
        /// </summary>
        public List<char> PossibleStates { get; private set; }

        private Dictionary<char, NextStateRule> _rules;
        private List<List<char>> _grid;
        private List<List<Dictionary<char, int>>> _counts;
        private readonly List<List<List<(int, int)>>> _neighbors;
        private int _numCols;
        private int _numRows;
        private char _emptySpace;

        private NeighborCountingRules _neighborCountingRule = NeighborCountingRules.Adjacent;

        /// <summary>
        /// Initiate a new game by providing a set of rules for each tile
        /// type. The rule takes in a list of pairs describing the counts
        /// of neighbors of various tile types and then returns the next state.
        /// </summary>
        /// <param name="rules">The rules to build the game with</param>
        /// <param name="initialState">The intitial state of the board, assumed
        /// to be rectangular.</param>
        /// <param name="emptySpace">Which sort of tile is considered empty.</param>
        /// <param name="neighborCountingRule">The rule to use for counting tiles</param>
        public GenericLife(
            Dictionary<char, NextStateRule> rules,
            string initialState,
            char emptySpace,
            NeighborCountingRules neighborCountingRule)
        {
            PossibleStates = new List<char>(rules.Keys);
            _rules = rules;

            _grid = initialState.Trim().Split('\n')
                .Select(row => row.ToList())
                .ToList();

            _numRows = _grid.Count;
            _numCols = _grid[0].Count;

            _emptySpace = emptySpace;
            _neighborCountingRule = neighborCountingRule;

            _counts = Enumerable.Range(0, _numRows)
                .Select(_ => Enumerable.Range(0, _numCols)
                    .Select(_ => PossibleStates.ToDictionary(state => state, state => 0))
                    .ToList())
                .ToList();

            // cache the neighbors positions. probably doesn't help much
            // for the adjacent rule, but might for the neares trule
            _neighbors = Enumerable.Range(0, _numRows)
                .Select(i => Enumerable.Range(0, _numCols)
                    .Select(j => GetNeighbors(i, j).ToList())
                    .ToList())
                .ToList();
        }

        /// <summary>
        /// Move forward one unit in time
        /// </summary>
        public void Tick()
        {
            GetNeighborCounts();
            ApplyRules();
        }

        public override string ToString()
            => string.Join("\n", _grid.Select(row => string.Join("", row)));

        /// <summary>
        /// Continue until the state doesn't change from one update to the next.
        /// </summary>
        public void ContinueUntilFixed()
        {
            var state = ToString();
            while (true)
            {
                Tick();
                var nextState = ToString();
                if (nextState == state)
                {
                    break;
                }
                state = nextState;
            }
        }

        /// <summary>
        /// Count the neighors of each soace
        /// </summary>
        private void GetNeighborCounts()
        {
            for (var i = 0; i < _numRows; i++)
            {
                for (var j = 0; j < _numCols; j++)
                {
                    foreach (var state in PossibleStates)
                    {
                        _counts[i][j][state] = 0;
                    }
                    
                    foreach (var (nRow, nCol) in _neighbors[i][j])
                    {
                        var neighbor = _grid[nRow][nCol];
                        _counts[i][j][neighbor]++;
                    }
                }
            }
        }

        /// <summary>
        /// Apply the rules of life
        /// </summary>
        /// <param name="neighborCounts">The counts of neighbors, counted
        /// before the rules are applied.</param>
        private void ApplyRules()
        {
            for (var i = 0; i < _numRows; i++)
            {
                for (var j = 0; j < _numCols; j++)
                {
                    var currentState = _grid[i][j];
                    var counts = _counts[i][j];
                    var rule = _rules[currentState];
                    var nextState = rule(counts);
                    _grid[i][j] = nextState;
                }
            }
        }

        /// <summary>
        /// Get the neighbors for the given cell
        /// </summary>
        /// <param name="row">The row for the current cell</param>
        /// <param name="col">The columnf or the current cell</param>
        /// <returns>The locations of the neigbors as an enumerable of pairs (row, col)</returns>
        private IEnumerable<(int nRow, int nCol)> GetNeighbors(int row, int col)
        {
            switch (_neighborCountingRule)
            {
                case NeighborCountingRules.Adjacent:
                    foreach (var neighbor in GetNeighborsAdjacent(row, col))
                    {
                        yield return neighbor;
                    }
                    break;
                case NeighborCountingRules.Closest:
                    foreach (var neighbor in GetNeighborsClosest(row, col))
                    {
                        yield return neighbor;
                    }
                    break;
            }
        }

        /// <summary>
        /// Generator for adjacent neighbors
        /// </summary>
        private IEnumerable<(int nRow, int nCol)> GetNeighborsAdjacent(int row, int col)
        { 
            for (var deltaRow = -1; deltaRow <= 1; deltaRow++)
            {
                var nRow = row + deltaRow;
                if (nRow < 0 || nRow >= _numRows)
                {
                    continue;
                }

                for (var deltaCol = -1; deltaCol <= 1; deltaCol++)
                {
                    var nCol = col + deltaCol;
                    if (nCol < 0 || nCol >= _numCols)
                    {
                        continue;
                    }
                    if (row == nRow && col == nCol)
                    {
                        continue;
                    }
                    yield return (nRow, nCol);
                }
            }
        }

        /// <summary>
        /// Generator for closest neighbors
        /// </summary>
        private IEnumerable<(int nRow, int nCol)> GetNeighborsClosest(int row, int col)
        {
            for (var deltaRow = -1; deltaRow <= 1; deltaRow++)
            {
                for (var deltaCol = -1; deltaCol <= 1; deltaCol++)
                {
                    if (deltaRow == 0 && deltaCol == 0)
                    {
                        continue;
                    }

                    var nRow = row;
                    var nCol = col;

                    while (true)
                    {
                        nRow += deltaRow;
                        nCol += deltaCol;
                        if (nRow < 0 || nRow >= _numRows || nCol < 0 || nCol >= _numCols)
                        {
                            break;
                        }

                        if (_grid[nRow][nCol] != _emptySpace)
                        {
                            yield return (nRow, nCol);
                            break;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Count the number of tiles that are of the given state
        /// </summary>
        public int Count(char state)
            => _grid.Select(row => row.Count(cellState => cellState == state)).Sum();
    }
}
