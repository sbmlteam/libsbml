/*
 * This project is licensed under the MIT license. For more information see the
 * LICENSE file.
 */
#pragma once

// -----------------------------------------------------------------------------

#include <functional>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "maddy/blockparser.h"
#include "maddy/paragraphparser.h"

// -----------------------------------------------------------------------------

namespace maddy {

// -----------------------------------------------------------------------------

/**
 * TableParser
 *
 * Recognizes GitHub-flavored pipe tables:
 *
 * ```
 * | Header A | Header B |
 * | -------- | -------- |
 * | Cell A1  | Cell B1  |
 * ```
 *
 * A table starts with a row of `|`-separated cells immediately followed by
 * a separator row whose cells contain only `-`, with an optional leading
 * and/or trailing `:`. It ends at the first blank line, or at the end of
 * input.
 *
 * Since `IsStartingLine` only sees one line at a time, it can't yet tell a
 * table header from an ordinary line that happens to contain a `|`. If the
 * line after it turns out not to be a valid separator row, both lines are
 * handed off to a ParagraphParser instead, since they were never a table.
 *
 * @class
 */
class TableParser : public BlockParser
{
public:
  /**
   * ctor
   *
   * @method
   * @param {std::function<void(std::string&)>} parseLineCallback
   * @param {std::function<std::shared_ptr<BlockParser>(const std::string&
   * line)>} getBlockParserForLineCallback
   */
  TableParser(
    std::function<void(std::string&)> parseLineCallback,
    std::function<std::shared_ptr<BlockParser>(const std::string& line)>
      getBlockParserForLineCallback
  )
    : BlockParser(parseLineCallback, getBlockParserForLineCallback)
    , state(State::EXPECT_HEADER)
    , isFinished(false)
  {}

  /**
   * IsStartingLine
   *
   * A table can only start with a row that has at least one `|` separating
   * two cells. Whether it really is a table is only known once the
   * following line (the separator row) has been seen.
   *
   * @method
   * @param {const std::string&} line
   * @return {bool}
   */
  static bool IsStartingLine(const std::string& line)
  {
    return IsTableRow(line);
  }

  /**
   * AddLine
   *
   * Adding a line which has to be parsed.
   *
   * @method
   * @param {std::string&} line
   * @return {void}
   */
  void AddLine(std::string& line) override
  {
    if (this->fallbackParser)
    {
      this->fallbackParser->AddLine(line);

      if (this->fallbackParser->IsFinished())
      {
        this->result << this->fallbackParser->GetResult().str();
        this->isFinished = true;
      }

      return;
    }

    switch (this->state)
    {
      case State::EXPECT_HEADER:
        this->headerLine = line;
        this->state = State::EXPECT_SEPARATOR;
        return;

      case State::EXPECT_SEPARATOR:
        if (
          IsSeparatorRow(line) &&
          SplitRow(line).size() == SplitRow(this->headerLine).size()
        )
        {
          this->WriteHeader();
          this->state = State::IN_BODY;
        }
        else
        {
          this->FallBackToParagraph(line);
        }
        return;

      case State::IN_BODY:
        if (line.empty())
        {
          this->result << "</tbody></table>";
          this->isFinished = true;
        }
        else
        {
          this->WriteRow(line);
        }
        return;
    }
  }

  /**
   * IsFinished
   *
   * A table ends with a blank line, or at the end of input.
   *
   * @method
   * @return {bool}
   */
  bool IsFinished() const override { return this->isFinished; }

protected:
  bool isInlineBlockAllowed() const override { return false; }

  bool isLineParserAllowed() const override { return false; }

  void parseBlock(std::string&) override {}

private:
  enum class State { EXPECT_HEADER, EXPECT_SEPARATOR, IN_BODY };

  State state;
  bool isFinished;
  std::string headerLine;
  std::shared_ptr<BlockParser> fallbackParser;

  static bool IsTableRow(const std::string& line)
  {
    return line.find('|') != std::string::npos &&
           line.find_first_not_of(" \t") != std::string::npos;
  }

  static bool IsSeparatorRow(const std::string& line)
  {
    if (!IsTableRow(line))
    {
      return false;
    }

    static const std::regex cellRe("^:?-+:?$");

    for (const std::string& cell : SplitRow(line))
    {
      if (!std::regex_match(cell, cellRe))
      {
        return false;
      }
    }

    return true;
  }

  static std::vector<std::string> SplitRow(const std::string& line)
  {
    std::vector<std::string> cells;
    std::stringstream stream(line);
    std::string cell;

    while (std::getline(stream, cell, '|'))
    {
      Trim(cell);

      if (!cell.empty())
      {
        cells.push_back(cell);
      }
    }

    return cells;
  }

  static void Trim(std::string& str)
  {
    size_t first = str.find_first_not_of(" \t");

    if (first == std::string::npos)
    {
      str.clear();
      return;
    }

    size_t last = str.find_last_not_of(" \t");
    str = str.substr(first, last - first + 1);
  }

  void WriteHeader()
  {
    this->result << "<table><thead><tr>";

    for (std::string cell : SplitRow(this->headerLine))
    {
      this->parseLine(cell);
      this->result << "<th>" << cell << "</th>";
    }

    this->result << "</tr></thead><tbody>";
  }

  void WriteRow(const std::string& line)
  {
    this->result << "<tr>";

    for (std::string cell : SplitRow(line))
    {
      this->parseLine(cell);
      this->result << "<td>" << cell << "</td>";
    }

    this->result << "</tr>";
  }

  void FallBackToParagraph(const std::string& secondLine)
  {
    this->fallbackParser = std::make_shared<ParagraphParser>(
      [this](std::string& l) { this->parseLine(l); },
      nullptr,
      true
    );

    std::string first = this->headerLine;
    this->fallbackParser->AddLine(first);

    std::string second = secondLine;
    this->fallbackParser->AddLine(second);

    if (this->fallbackParser->IsFinished())
    {
      this->result << this->fallbackParser->GetResult().str();
      this->isFinished = true;
    }
  }
}; // class TableParser

// -----------------------------------------------------------------------------

} // namespace maddy
