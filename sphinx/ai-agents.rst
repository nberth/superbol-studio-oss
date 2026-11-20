Tools for AI Agents
===================

*SuperBOL* gives AI agents (e.g. coding assistants) tools to find their way in
COBOL programs, from what its LSP server knows of them:

* **outline** of a program: its divisions, sections, paragraphs, and data items,
  with their line ranges;
* **symbol search**: programs, sections, paragraphs, and data items of the
  workspace, by name;
* **problems**: errors and warnings that SuperBOL reports;
* **project settings**: dialect, source format, and where copybooks are;
* **control flow** between the paragraphs and sections of a program.

Outputs are text meant for language models: agents can use these tools instead
of reading whole files, which COBOL programs make costly.

In Visual Studio Code
---------------------

With Visual Studio Code 1.101 or later, the extension makes the tools
available to agents, e.g. Copilot in agent mode.  They can also be referred to
in prompts: :code:`#cobolOutline`, :code:`#cobolSymbol`,
:code:`#cobolProblems`, :code:`#cobolSettings`, and :code:`#cobolControlFlow`.

Other agents: MCP server
------------------------

For other agents, :code:`superbol-free mcp` runs a `Model Context Protocol`_
(MCP) server that provides the same tools on its standard input and output.
The :code:`superbol-free` executable is built along with the LSP server; the
Visual Studio Code extension also ships it, in the :file:`_dist` directory of
the installed extension (e.g. :file:`superbol-free-linux-x64`).

Agents start the server themselves, from the root directory of the workspace
by default.  For instance, with Claude Code:

.. code-block:: shell

    claude mcp add superbol -- superbol-free mcp

Most other agents take a JSON configuration such as:

.. code-block:: json

    {
      "mcpServers": {
        "superbol": { "command": "superbol-free", "args": ["mcp"] }
      }
    }

The server takes the following options:

* :code:`--workspace DIR`: root directory of the workspace (the current
  directory by default);
* :code:`--client-settings JSON`: settings for projects that have no
  :file:`superbol.toml` file, as Visual Studio Code gives them with its
  :code:`superbol.cobol` settings (e.g. :code:`'{"dialect": "ibm"}'`);
* :code:`--encoding ENCODING`: decode files as Visual Studio Code does with
  this value of its :code:`files.encoding` setting;
* :code:`--storage-directory DIR`, :code:`--no-caching`, and
  :code:`--force-syntax-diagnostics`, as for :code:`superbol-free lsp`.

Symbol search and problems only cover the files whose analysis SuperBOL has
cached: those that the Visual Studio Code extension has analyzed, or that
:code:`superbol-free cache analyze --walk-project` analyzes.  For the MCP server
to find these analyses, give it the same :code:`--storage-directory`,
:code:`--client-settings`, and :code:`--encoding` options.  The server stores
the analyses of the files its tools open, but never rewrites caches as a
whole, so that it can run along with an editor.

.. _Model Context Protocol: https://modelcontextprotocol.io
