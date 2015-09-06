// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using Microsoft.VisualStudio.InteractiveWindow.Commands;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Projection;
using Moq;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.VisualStudio.InteractiveWindow.UnitTests
{
    public class HistoryTests : IDisposable
    {
        private readonly InteractiveWindowTestHost _testHost;
        private readonly ITextBuffer _buffer;
        private readonly History _history;

        public HistoryTests()
        {
            _testHost = new InteractiveWindowTestHost();
            _buffer = _testHost.ExportProvider.GetExport<ITextBufferFactoryService>().Value.CreateTextBuffer();
            _history = new History();
        }

        void IDisposable.Dispose()
        {
            _testHost.Dispose();
        }

        [Fact]
        public void TestPrevious()
        {
            AddEntries("1", "2", "3");

            Test(
                new Step(() => _history.GetPrevious(null), "3"),
                new Step(() => _history.GetPrevious(null), "2"),
                new Step(() => _history.GetPrevious(null), "1"),
                new Step(() => _history.GetPrevious(null), null),
                new Step(() => _history.GetPrevious(null), null));
        }

        [Fact]
        public void TestNext()
        {
            AddEntries("1", "2", "3");

            Test(
                new Step(() => _history.GetNext(null), null),
                new Step(() => _history.GetNext(null), null),
                new Step(() => _history.GetPrevious(null), "3"),
                new Step(() => _history.GetPrevious(null), "2"),
                new Step(() => _history.GetPrevious(null), "1"),
                new Step(() => _history.GetPrevious(null), null),
                new Step(() => _history.GetNext(null), "2"),
                new Step(() => _history.GetNext(null), "3"),
                new Step(() => _history.GetNext(null), null),
                new Step(() => _history.GetNext(null), null));
        }

        private void Test(params Step[] steps)
        {
            int i = 0;
            foreach (var step in steps)
            {
                var actualEntry = step.Func();
                var expected = step.ExpectedText;
                var actual = actualEntry?.Text;
                if (expected != actual)
                {
                    Assert.False(true, $"Step {i}: expected '{expected ?? "null"}', but found '{actual ?? "null"}'");
                }
                i++;
            }
        }

        private void AddEntries(params string[] entries)
        {
            var oldLength = _buffer.CurrentSnapshot.Length;

            foreach (var entry in entries)
            {
                AddEntry(entry);
            }

            Assert.Equal(string.Join(Environment.NewLine, entries) + Environment.NewLine, _buffer.CurrentSnapshot.GetText().Substring(oldLength));
        }

        private void AddEntry(string entry)
        {
            var oldLength = _buffer.CurrentSnapshot.Length;
            var snapshot = _buffer.Insert(oldLength, entry);
            var snapshotSpan = new SnapshotSpan(snapshot, new Span(oldLength, entry.Length));
            _history.Add(snapshotSpan);
            _buffer.Insert(snapshot.Length, Environment.NewLine);
        }

        private struct Step
        {
            public readonly Func<History.Entry> Func;
            public readonly string ExpectedText;

            public Step(Func<History.Entry> func, string expectedText)
            {
                Func = func;
                ExpectedText = expectedText;
            }
        }
    }
}