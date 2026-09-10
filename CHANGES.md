# coq-lsp 0.2.6: unreleased
--------------------------

 - [fleche] New central state store (`Fleche.States`): Rocq states are
   owned in one place and referred to by id, so the memo caches no
   longer retain them. Documents and protocol handles pin the states
   they name; everything else is a cache, bounded by the new
   `state_cache_size` setting, with the least recently used states
   dropped past it (@remix7531)
 - [petanque] New requests `petanque/state/free`,
   `petanque/state/stats` and `petanque/cache/trim`, letting a
   long-running client release states instead of growing until the
   process is killed; protocol version is now 4 (@remix7531)
 - [petanque] State handles are ids in the store: the same id always
   means the same state, serialising the same state twice usually
   yields the same id, and a state a client has not freed is never
   dropped. Clients should free every distinct id they were handed
   (@remix7531)
 - [fleche] `Doc.run` takes a new mandatory `files` argument, and runs
   `Require` sentences through the `Require` cache instead of the
   sentence cache, which also serves `petanque/run` and goals with
   `pretac`; the `Require` cache is cleared on a workspace update
   (@remix7531)

# coq-lsp 0.2.5: New epoch
--------------------------

 - [build] [wasm] [code] bump esbuild from 0.16 to 0.25, miscellaneous
   npm dependencies bump (@ejgallego, #1033)
 - [meta] Coq -> Rocq documentation rename (@ejgallego, #1034)
 - [fleche] Support `\begin{rocq}` in literate Tex files (@ejgallego,
   #1034)
 - [fleche] Fix crash in `coq/trimCaches` notification (#1035,
   @ejgallego, reported by @gbdrt)
 - [controller] [goals] Fix a couple of bugs where goal printing
   didn't respect the `pp_format` setting, introduced in
   #668. (@ejgallego, #1037, fixes #1030, thanks to Will Thomas for
   the report)
 - [opam] Require memprof-limits in OCaml >= 5.3 (@ejgallego, #1038)
 - [ocaml] Support for OCaml 5.4, drop support for OCaml 5.0, 5.1, and
   5.2, drop support for Coq 8.17, 8.18, 8.19 (@ejgallego, #1039)
 - [vscode] New command "Serialize Sentence at Point" that will print
   the AST of the Rocq sentence at point (@ejgallego, #1048)
 - [petanque] New request `petanque/run_at_pos` (@ejgallego, #1049)
 - [petanque] `petanque/get_state_at_pos` will default to the previous
   node state if there is no Rocq node at the current point
   (@ejgallego, #1049)
 - [layout-engine] Support for notations with binders (@ejgallego,
   #1050)
 - [layout-engine] Add background color for each box kind (@ejgallego,
   #1050)
 - [compiler] Fix handling of literate files (@ejgallego, #1056,
   reported by @jim-portegies)
 - [rocq] Create output directory on .vo file save if it doesn't
   exists. This is very useful in the web context where we don't yet
   have a proper virtual FS setup, thus making the "Save .vo file"
   command fail (@ejgallego, #1054)
 - [web worker] Add LSP root to Rocq's loadpath, this makes .vo file
   loading work even when no worker FS setup could happen (@ejgallego,
   #1054)
 - [fleche] New `coq/workspace_update` notification, which tells
   Flèche that `.vo` files in the workspace may have changed. This
   will make coq-lsp pick up changes in the filesystem and restart
   checking of Rocq's `Require`s when needed. The algorithm is not
   smart at all yet, as it invalidates all the requires for all open
   files. (@ejgallego, @helguo, #1059)
 - [goals] New option (LSP, petanque, API) to display and retrieve
   goals without the compacted context. See protocol docs for more
   information. (@ejgallego, alexJ, #1065, cc #1043)
 - [fleche] [rocq] New options and data fields to expose document
   comments. This is experimental, pass the `--record_comments` flags
   to `rocq-lsp`, `fcc`, or `pet-server` to enable it. Once this flag
   is passed, comments for a document will be stored in the new
   `doc.comments` field. (@ejgallego, alexJ, #1069, fixes #353)
 - [hover] New debug option `show_pr_vernac_on_hover`, that will
   re-print the sentence under point using the Rocq printer
   (@ejgallego, alexJ, #1070)
 - [petanque] New methods `proof_info` and `proof_info_at_pos` that
   return information about the current proof. As of today they return
   the proof name, text, range, ... (@DikieDick, @ejgallego, #1051)
 - [petanque] New method `list_notations_in_statement` which returns
   an analysis of notations appearing inside a Rocq statement
   (@JulesViennotFranca, @ejgallego, #1017)
 - [tools] New tool `rocq-checkdecls` for Rocq blueprints, inspired by
   the Lean version (#785, @ejgallego, Andrej Bauer)
 - [plugins] New demo plugin "baseline", that tries to proof existing
   lemmas using a set of pre-fixed tactics (@ejgallego, Shachar
   Itzhaky, Eytan Singher, #799)
 - [lsp] [rocq] Example of Ltac2 AST analysis using serlib's Analzyer
   infra (@ejgallego, @jim-portegies, @DikieDick, #1058)
 - [lsp controller] Log internal errors to user-facing log
   (@ejgallego, #1074)
 - [doc] Example typescript client connecting to the WASM-based lsp
   server (@ejgallego, #626)
 - [hover] When the `show_doc_on_hover` option is enabled, hover will
   show coqdoc documentation when hovering on identifiers, using some
   heuristics to infer it from the comment just before
   it. Documentation is treated as Markdown. This feature is
   experimental and limited to documentation comments on the same file.
   (@ejgallego, #590)
 - [plugins] New plugin to dump document data (@ejgallego, Stefania
   Dumbrava, #1075)
 - [fcc] New option `--trace-file` that will generate trace data for
   visualizing with perfetto.dev (@ejgallego, AlexJ, Gaëtan Gilbert,
   #1076)
 - [fcc] New options `--save_vof` and `--load_vof` that can save a
   Fleche version of a document to a `.vof` file, thus avoiding `.vo`
   recompilation for example when calling `fcc --plugin=...` for a
   given file (@ejgallego, Alexj, #1077)
 - [vscode] Add config manager for handling client configuration
   changes in the infoview. Add configuration option for number of
   messages displayed (@Durbatuluk1701, #1067)
 - [wasm] Update interrupt patch to account for timeouts (@ejgallego,
   Shachar Itzhaky, #1078)

# coq-lsp 0.2.4: (W)Activation
------------------------------

 - [js] [deps] Bump to findlib 1.9.8, use vanilla API for loading and
   remove our own local wrapper (@ejgallego, #975).
 - [petanque] New `petanque/ast` and `petanque/ast_at_pos`
   (@ejgallego, @JulesViennotFranca, #980)
 - [serlib] Support for generic Ast analyzers. This opens the door to
   many feature requests such as syntax coloring, dependency
   extraction, etc... (@ejgallego, @JulesViennotFranca, #981)
 - [fleche] Support "rocq" markdown delimiters in .mv files
   (@ejgallego, #987)
 - [workspace] Support _RocqProject (@ejgallego, #988, fixes #934)
 - [lsp] [getDocument] Allow to get goals in one shot. We also
   refactor the response type to accommodate different
   meta-data. Note: (!) breaking change. (@ejgallego, #985, fixes
   #862, thanks to the Alectryon team)
 - [lsp] Better error handling in URI parsing (@ejgallego, #994, thanks to
   Adrien from Zulip)
 - [lsp] Better protocol-level handling for our non-standard `Lang.Point`
   and `Lang.Diagnostic` types, via global flags that allow us to
   choose the input/output representation for non-standard field such
   as [Point.offset]. This ensures that leaks of these non-standard
   fields are rarer. (@ejgallego, #995, cc #279, cc #2, thanks to
   Adrien from Zulip)
 - [lsp] [completion] Rework completion configuration into a
   `coq-lsp.completion` json object. The `unicode_completion` setting
   is now deprecated, and has been replaced by
   `completion.unicode.enable` (@ejgallego, #993)
 - [lsp] [completion] [vscode] Unicode completion commit characters
   are now configurable via the server setting variable
   `completion.unicode.commit_chars`. (@Durbatuluk1701, #993)
 - [goals] [config] New (global) option for goal display method
   `proof/goals`: `messages_follow_goal`. If `true`, `proof/goals`
   will show errors and messages for the same sentence goals are
   shown; if `false`, it will always show errors and messages for the
   specified `position`, if there is a Rocq sentence at hand
   (@jpoiret, @ejgallego, #999, fixes: #941)
 - [coq] Install full state before parsing. Before we did only
   `Pcoq.unfreeze` but that is not enough, in particular the call to
   `get_default_proof_mode` will not be correct (@ejgallego, @pimotte,
   #1011, fixes #656)
 - [misc] Don't depend on Jane Street's base (@patrick-nicodemus
   @ejgallego, #1004)
 - [wasm worker] Add WebAssembly based worker based on waCoq. This is
   now the default for the .vsix binary build. For now, we include
   Rocq's Stdlib and Waterproof (@corwin-of-amber, @ejgallego,
   @pimotte, #1008, cc #833, fixes #907, fixes #908, fixes #913)
 - [opam] Added `x-maintenance-intent` intent field. (@ejgallego,
   #1020)
 - [lsp] [didOpen] `languageId` now takes priority over uri extension
   in LSP `didOpen`. (@ejgallego, #1021, fixes #1005)
 - [coq] incorporate experimental `coq-layout-engine` printer, both in
   client and server parts (@ejgallego, #668, see also #72 and
   https://github.com/jscoq/jscoq/pull/282 )
 - [lsp] [code] New notification `$/coq/executionInformation` which
   will signal clients when rocq-lsp does intent to start to execute a
   sentence. Experimentally, this is used to provide a red glow on
   long-running commands in coq-lsp/VSCode, to provide better user
   feedback on long-running commands (@ejgallego, suggested by
   @jpoiret, #1002)
 - [lsp] [outline] Support `Notation`, `Ltac` and `Ltac Notation` in
   outline entries (@ejgallego, #1025, fixes #632)

# coq-lsp 0.2.3: Barrage
------------------------

 - [fleche] fix quick fixes for errors being lost due to incorrect
   handling of `send_diags_extra_data` (@ejgallego, #850)
 - [vscode] Syntax highlighting for Coq 8.17-8.20 (@4ever2, #872)
 - [build] Adapt to Coq -> Rocq renaming (@ejgallego, @proux, #879)
 - [js worker] Update js_of_ocaml to 5.9.1 , thanks a lot to Hugo
   Heuzard for longstanding continued support of the jsCoq and coq-lsp
   projects (@ejgallego, @hhugo, #881)
 - [js worker] Update stubs (@ejgallego, @hhugo, #881)
 - [js worker] Fix build for Coq -> Rocq renaming and stdlib split
   (@ejgallego, #881)
 - [general] Adapt to Coq -> Rocq renaming (@ejgallego, @SkySkimmer,
   #883)
 - [general] [js] Adapt to Rocq stdlib split (@ejgallego, #890)
 - [ci] Bump setup-ocaml to v3 (@ejgallego, #890)
 - [ci] [windows] Use Opam 2.2 to build on windows (@ejgallego, #815,
   #890)
 - [petanque] `petanque/start` now fails when the theorem was parsed
   but not successfully executed (@ejgallego, reported by @gbdrt,
   #901, fixes #886)
 - [ci] Test Ocaml 5.3 (@ejgallego, #904)
 - [js worker] Add Shachar Itzhaky's trampoline patch; this greatly
   reduces the Stack Overflow in the proof engine (@ejgallego,
   @corwin-of-amber, #905)
 - [js worker] [build] Include Coq WaterProof in the default Web
   Worker build (@ejgallego, waterproof team, #905, closes #888)
 - [vscode] [web] Fix web extension not exporting the coq-lsp
   extension API (@ejgallego, reported by @amblafont, #911, fixes
   #877)
 - [build] [general] Rename our internal `Lsp` library to
   `Fleche_lsp`; this should help avoiding conflicts with the OCaml
   `lsp` library (@ejgallego, reported by @blackbird1128, #912, fixes
   #861)
 - [workspace] Remove support legacy ML-search path semantics. These
   were basically unused since Coq 8.16. As a consequence, `coq-lsp` /
   `fcc` don't accept the `-I` flag anymore, use `OCAMLPATH` or the
   `--ocamlpath=` option to pass extra `findlib` paths. We still
   respect the -I flag in `_CoqMakefile` (@ejgallego, #916)
 - [lsp] [debug] Respect `$/setTrace` call , refactor logging system,
   and allow file logging of protocol traces again (@ejgallego, #919,
   fixes #868)
 - [coq] Support Coq relocatable mode (@SkySkimmer, #891)
 - [ci] [deps] Remove support for OCaml 4.12 and 4.13, following
   upstream's coq/coq#20576 Note that these compiler versions have
   been unsupported for a long time, please upgrade (@ejgallego, #951)
 - [hover] New option `show_state_hash_on_hover` that displays state
    hash on hover for debug (@ejgallego, #954)
 - [doc] [faq] Updated FAQ to account for VSCoq 2 release in 2023,
   thanks to Patrick Nicodemus for pointing out the outdated
   documentation (@ejgallego, #846, fixes #817)
 - [vscode] [macos] Resolve keybinding conflict with Cmd+N and
   Cmd+Enter, we now use Alt+N and Alt+Shift+Enter, (Andrei
   Listochkin, #926)
 - [rocq] [fleche] Disable memprof-limits interruption backend by
   default, as released Rocq versions are not safe yet. If you want to
   enable it, you can still do it with the `--int_backend=Mp` command
   line option (@ejgallego, #957, fixes #857, reported by @dariusf,
   cc: rocq-prover/rocq#19177)
 - [lsp] [controller] Include Rocq feedback on request errors, using
   the optional `data` field. This is useful to still be able to
   obtain feedback messages such as debug messages even when a request
   fails. This also opens the door to better protocol handling and
   petanque integration (@ejgallego, #959, #961)
 - [petanque] Add feedback field to `Run_result.t`, this is important
   for many use cases. We also return feedback on petanque errors.
   (@ejgallego, @JulesViennotFranca, #960)
 - [petanque] new `get_state_at_pos` and `get_root_state` calls, that
   allow to retrieve a petanque proof state from position
   (@JulesViennotFranca, @ejgallego, #962)
 - [doc] [petanque] Document petanque v1, improve readme (@ejgallego,
   #963)
 - [plugin] [astdump] Make the JSON and SEXP output into a line per
   object by default (@blackbird1128, @ejgallego, #874)
 - [doc] [emacs] [protocol] Improve documentation for `proof/goals`,
   add link to official emacs mode by Josselin Poiret (@ejgallego,
   #969, thanks to @jpoiret, cc: #941)
 - [goals] Include `range` in `proof/goals` answer. This is useful for
   clients willing to do highlighting (@ejgallego, @jpoiret, #970)

# coq-lsp 0.2.2: To Virtual or not To Virtual
---------------------------------------------

 - [vscode] Expand selectors to include `vscode-vfs://` URIs used in
   `github.dev`, document limited virtual workspace support in
   `package.json` (@ejgallego, #849)

# coq-lsp 0.2.1: Click !
------------------------

 - [deps] Bump toolchain so minimal `ppxlib` is 0.26, in order to fix
   some `ppx_import` oddities. This means our lower bound for the Jane
   Street packages is now `v0.15`, which should be fine for the
   foreseeable future (@ejgallego, #813)
 - [workspace] [coq] Support _CoqProject arguments `-type-in-type` and
   `-allow-rewrite-rules` (for 8.20) (@ejgallego, #819)
 - [serlib] Support for ltac2_ltac1 plugin (@ejgallego, #820)
 - [serlib] Fix Ltac2 AST piercing bug, add test case that should help
   in the future (@ejgallego, @jim-portegies, #821)
 - [fleche] [8.20] understand rewrite rules and symbols on document
   outline (@ejgallego, @Alizter, #825, fixes #824)
 - [fleche] [coq] support `Restart` meta command (@ejgallego,
   @Alizter, #828, fixes #827)
 - [fleche] [plugins] New plugin example `explain_errors`, that will
   print all errors on a file, with their goal context (@ejgallego,
   #829, thanks to @gmalecha for the idea, c.f. Coq issue 19601)
 - [fleche] Highlight the full first line of the document on
   initialization error (@ejgallego, #832)
 - [fleche] [jscoq] [js] Build worker version of `coq-lsp`. This
   provides a full working Coq enviroment in `vscode.dev`. The web
   worker version is build as an artifact on CI (@ejgallego
   @corwin-of-amber, #433)
 - [hover] Fix universe and level printing in hover (#839, fixes #835
   , @ejgallego , @Alizter)
 - [fleche] New immediate request serving mode. In this mode, requests
   are served with whatever document state we have. This is very
   useful when we are not in continuous mode, and we don't have a good
   reference as to what to build, for example in
   `documentSymbols`. The mode actually works pretty well in practice
   as often language requests will come after goals requests, so the
   info that is needed is at hand. It could also be tried to set the
   build target for immediate requests to the view hint, but we should
   see some motivation for that (@ejgallego, #841)
 - [lsp] [diagnostics] (! breaking change) change type of diagnostics
   extra data from list to named record (@ejgallego, #843)
 - [lsp] Implement support for `textDocument/codeAction`. For now, we
   support Coq's 8.21 `quickFix` data (@ejgallego, #840, #843, #845)
 - [petanque] Fix bug for detection of proof finished in deep stacks
   (@ejgallego, @gbdrt, #847)
 - [goals request] allow multiple Coq sentences in `command`. This is
   backwards compatible in the case that commands do not error, and
   users were sending just one command. (@ejgallego, #823, thanks to
   CoqPilot devs and G. Baudart for feedback)
 - [goals request] (! breaking) fail the request if the commands in
   `command` fail (@ejgallego, #823)

# coq-lsp 0.2.0: From Green to Blue
-----------------------------------

 - [deps] merge serlib into coq-lsp. This allow us to drop the SerAPI
   dependency, and will greatly easy the development of tools that
   require AST manipulation (@ejgallego, #698)
 - [fleche] Remove 8.16 compatibility layer (@ejgallego, #747)
 - [fleche] Preserve view hint across document changes. With this
   change, we get local continuous checking mode when the view-port
   heuristic is enabled (@ejgallego, #748)
 - [memo] More precise hashing for Coq states, this improves cache
   performance quite a bit (@ejgallego, #751)
 - [fleche] Enable sharing of `.vo` file parsing. This enables better
   sharing, achieving an almost 50% memory reduction for example when
   opening all of HoTT .v files (@ejgallego, @SkySkimmer, @bhaktishh,
   #744)
 - [memo] Provide API to query Hashtbl stats (@ejgallego, #753)
 - [nix] Add `pet-server` deps to flake.nix (Léo Stefanesco, #754)
 - [coq-lsp] Fix crash on `--help` option (Léo Stefanesco, @ejgallego,
   #754)
 - [vscode] Fix focus race when a Coq file is in column 2 (@ejgallego,
   #755, cc: #722, #725)
 - [hover] Show input howto for unicode characters on hover
   (@ejgallego, Léo Stefanesco, #756)
 - [lsp] [definition] Support for jump to definition across workspace
   files. The location information is obtained from `.glob` files, so
   it is often not perfect. (@ejgallego, #762, fixes #317)
 - [lsp] [hover] Show full name and provenance of identifiers
   (@ejgallego, #762)
 - [lsp] [definition] Try also to resolve and locate module imports
   (@ejgallego, #764)
 - [code] Don't start server on extension activation, unless an editor
   we own is active. We also auto-start the server if a document that
   we own is opened later (@ejgallego, #758, fixes #750)
 - [petanque] Allow `init` to be called multiple times (@ejgallego,
   @gbdrt, #766)
 - [petanque] Faster query for goals status after `run_tac`
   (@ejgallego, #768)
 - [petanque] New parameter `pre_commands` to `start` which allows
   instrumenting the goal before starting the proof (@ejgallego, Alex
   Sanchez-Stern #769)
 - [petanque] New `http_headers={yes,no}` parameter for `pet` json
   shell, plus some improvements on protocol handling (@ejgallego,
   #770)
 - [petanque] Make agent agnostic of environment, allowing embedding
   inside LSP (@ejgallego, #771)
 - [diagnostics] Ensure extra diagnostics info is present in all
   errors, not only on those sentences that did parse successfully
   (@ejgallego, Diego Rivera, #772)
 - [hover] New option `show_universes_on_hover` that will display
   universe data on hover (@ejgallego, @SkySkimmer, #666)
 - [hover] New plugin `unidiff` that will elaborate a summary of
   universe data a file, in particular regarding universes added at
   `Qed` time (@ejgallego, #773)
 - [fleche] Support meta-command `Abort All` (@ejgallego, #774, fixes
   #550)
 - [petanque] Allow memoization control on `petanque/run` via a new
   parameter `memo` (@ejgallego, #780)
 - [lsp] [petanque] Allow acces to `petanque` protocol from the lsp
   server (@ejgallego, #778)
 - [petanque] Always initialize a workspace. This made `pet` crash if
   `--root` was not used and client didn't issue the optimal
   `setWorkspace` call (#782, @ejgallego, @gbdrt)
 - [lsp] [petanque] New methods `state/eq` and `state/hash`; this
   allows clients to implement a client-side hash; equality is
   configurable with different methods; moreover, `petanque/run` can
   compute some extra data like state hashing without a round-trip
   (@ejgallego @gbdrt, #779)
 - [petanque] New methods to hash proof states; use proof state hash
   by default in petanque agent (@ejgallego, @gbdrt, #808)
 - [petanque] New shell method `petanque/toc` that returns a document
   outline in LSP-style (@ejgallego, #794)

# coq-lsp 0.1.10: Hasta el 40 de Mayo _en effect_...
----------------------------------------------------

 - [code] Add `.v.tex` file extension to contributed language support
   (@ejgallego, #740).
 - [code] Don't show the panel on extension activation (@ejgallego,
   #741, fix #737)

# coq-lsp 0.1.9: Hasta el 40 de Mayo...
---------------------------------------

 - new option `show_loc_info_on_hover` that will display parsing debug
   information on hover; previous flag was fixed in code, which is way
   less flexible. This also fixes the option being on in 0.1.8 by
   mistake (@ejgallego, #588)
 - hover plugins can now access the full document, this is convenient
   for many use cases (@ejgallego, #591)
 - fix hover position computation on the presence of Utf characters
   (@ejgallego, #597, thanks to Pierre Courtieu for the report and
   example, closes #594)
 - fix activation bug that prevented extension activation for `.mv`
   files, see discussion in the issues about the upstream policy
   (@ejgallego @r3m0t, #598, cc #596, reported by Théo Zimmerman)
 - require VSCode >= 1.82 in package.json . Our VSCode extension uses
   `vscode-languageclient` 9 which imposes this. (@ejgallego, #599,
   thanks to Théo Zimmerman for the report)
 - `proof/goals` request: new `mode` parameter, to specify goals
   after/before sentence display; renamed `pretac` to `command`, as to
   provide official support for speculative execution (@ejgallego, #600)
 - fix some cases where interrupted computations where memoized
   (@ejgallego, #603)
 - [internal] Flèche [Doc.t] API will now absorb errors on document
   update and creation into the document itself. Thus, a document that
   failed to create or update is still valid, but in the right failed
   state. This is a much needed API change for a lot of use cases
   (@ejgallego, #604)
 - support OCaml 5.1.x (@ejgallego, #606)
 - update progress indicator correctly on End Of File (@ejgallego,
   #605, fixes #445)
 - [plugins] New `astdump` plugin to dump AST of files into JSON and
   SEXP (@ejgallego, #607)
 - errors on save where not properly caught (@ejgallego, #608)
 - switch default of `goal_after_tactic` to `true` (@Alizter,
   @ejgallego, cc: #614)
 - error recovery: Recognize `Defined` and `Admitted` in lex recovery
   (@ejgallego, #616)
 - completion: correctly understand UTF-16 code points on completion
   request (Léo Stefanesco, #613, fixes #531)
 - don't trigger the goals window in general markdown buffer
   (@ejgallego, #625, reported by Théo Zimmerman)
 - allow not to postpone full document requests (#626, @ejgallego)
 - new configuration value `check_only_on_request` which will delay
   checking the document until a request has been made (#629, cc: #24,
   @ejgallego)
 - fix typo on package.json configuration section (@ejgallego, #645)
 - be more resilient with invalid _CoqProject files (@ejgallego, #646)
 - support for Coq 8.16 has been abandoned due to lack of dev
   resources (@ejgallego, #649)
 - new option `--no_vo` for `fcc`, which will skip the `.vo` saving
   step. `.vo` saving is now an `fcc` plugins, but for now, it is
   enabled by default (@ejgallego, #650)
 - depend on `memprof-limits` on OCaml 4.x (@ejgallego, #660)
 - bump minimal OCaml version to 4.12 due to `memprof-limits`
   (@ejgallego, #660)
 - monitor all Coq-level calls under an interruption token
   (@ejgallego, #661)
 - interpret require thru our own custom execution env-aware path
   (@bhaktishh, @ejgallego, #642, #643, #644)
 - new `coq-lsp.plugin.goaldump` plugin, as an example on how to dump
   goals from a document (@ejgallego @gbdrt, #619)
 - new trim command (both in the server and in VSCode) to liberate
   space used in the cache (@ejgallego, #662, fixes #367 cc: #253 #236
   #348)
 - fix Coq performance view display (@ejgallego, #663, regression in
   #513)
 - Added new heatmap feature allowing timing data to be seen in the
   editor. Can be enabled with the `Coq LSP: Toggle heatmap`
   command. Can be configured to show memory usage. Colors and
   granularity are configurable. (@Alizter and @ejgallego, #686,
   grants #681).
 - allow more than one input position in `selectionRange` LSP call
   (@ejgallego, #667, fixes #663)
 - new VSCode commands to allow to move one sentence backwards /
   forward, this is particularly useful when combined with lazy
   checking mode (@ejgallego, #671, fixes #263, fixes #580)
 - VSCode commands `coq-lsp.sentenceNext` / `coq-lsp.sentencePrevious`
   are now bound by default to `Alt + N` / `Alt + P` keybindings
   (@ejgallego, #718)
 - change diagnostic `extra` field to `data`, so we now conform to the
   LSP spec, include the data only when the `send_diags_extra_data`
   server-side option is enabled (@ejgallego, #670)
 - include range of full sentence in error diagnostic extra data
   (@ejgallego, #670 , thanks to @driverag22 for the idea, cc: #663).
 - The `coq-lsp.pp_type` VSCode client option now takes effect
   immediately, no more need to restart the server to get different
   goal display formats (@ejgallego, #675)
 - new public VSCode extension API so other extensions can perform
   actions when the user request the goals (@ejgallego, @bhaktishh,
   #672, fixes #538)
 - Support Visual Studio Live Share URIs better (`vsls://`), in
   particular don't try to display goals if the URI is VSLS one
   (@ejgallego, #676)
 - New `InjectRequire` plugin API for plugins to be able to instrument
   the default import list of files (@ejgallego @corwin-of-amber,
   #679)
 - Add `--max_errors=n` option to `fcc`, this way users can set
   `--max_errors=0` to imitate `coqc` behavior (@ejgallego, #680)
 - Fix `fcc` exit status when checking terminates with fatal errors
   (@ejgallego, @Alizter, #680)
 - Fix install to OPAM switches from `main` branch (@ejgallego, #683,
   fixes #682, cc #479 #488, thanks to @Hazardouspeach for the report)
 - New `--int_backend={Coq,Mp}` command line parameter to select the
   interruption method for Coq (@ejgallego, #684)
 - Update `package-lock.json` for latest bugfixes (@ejgallego, #687)
 - Update Nix flake enviroment (@Alizter, #684 #688)
 - Update `prettier` (@Alizter @ejgallego, #684 #688)
 - Store original performance data in the cache, so we now display the
   original timing and memory data even for cached commands (@ejgallego, #693)
 - Fix type errors in the Performance Data Notifications (@ejgallego,
   @Alizter, #689, #693)
 - Send performance performance data for the full document
   (@ejgallego, @Alizter, #689, #693)
 - Better types `coq/perfData` call (@ejgallego @Alizter, #689)
 - New server option to enable / disable `coq/perfData` (@ejgallego, #689)
 - New client option to enable / disable `coq/perfData` (@ejgallego, #717)
 - The `coq-lsp.document` VSCode command will now display the returned
   JSON data in a new editor (@ejgallego, #701)
 - Update server settings on the fly when tweaking them in VSCode.
   Implement `workspace/didChangeConfiguration` (@ejgallego, #702)
 - [Coq API] Add functions to retrieve list of declarations done in
   .vo files (@ejgallego, @eytans, #704)
 - New `petanque` API to interact directly with Coq's proof
   engine. (@ejgallego, @gbdrt, Laetitia Teodorescu #703, thanks to
   Alex Sanchez-Stern for many insightful feedback and testing)
 - New `petanque` JSON-RPC `pet.exe`, which can be used à la SerAPI
   to perform proof search and more (@ejgallego, @gbdrt, #705)
 - New `pet-server.exe` TCP server for keep-alive sessions (@gbdrt,
   #697)
 - Always dispose UI elements. This should improve some strange
   behaviors on extension restart (@ejgallego, #708)
 - [code] Added new heatmap feature allowing timing data to be seen in
   the editor. Can be enabled with the `Coq LSP: Toggle heatmap`
   comamnd. Can be configured to show memory usage. Colors and
   granularity are configurable. (@Alizter and @ejgallego, #686,
   grants #681).
 - [server] Support Coq meta-commands (Reset, Reset Initial, Back)
   They are actually pretty useful to hint the incremental engine to
   ignore changes in some part of the document (@ejgallego, #709)
 - JSON-RPC library now supports all kind of incoming messages
   (@ejgallego, #713)
 - [code/server] New `coq/viewRange` notification, from client to
   server, than hints the scheduler for the visible area of the
   document; combined with the new lazy checking mode, this provides
   checking on scroll, a feature inspired from Isabelle IDE
   (@ejgallego, #717)
 - [code] Have VSCode wait for full LSP client shutdown on server
   restart. This fixes some bugs on extension restart (finally!)
   (@ejgallego, #719)
 - [code] Center the view if cursor goes out of scope in
   `sentenceNext/sentencePrevious` (@ejgallego, #718)
 - Switch Flèche range encoding to protocol native, this means UTF-16
   code points for now (Léo Stefanesco, @ejgallego, #624, fixes #620,
   #621)
 - Give `Goals` panel focus back if it has lost it (in case of
   multiple panels in the second viewColumn of Vscode) whenever
   user navigates proofs (@Alidra @ejgallego, #722, #725)
 - `fcc`: new option `--diags_level` to control whether Coq's notice
   and info messages appear as diagnostics
 - [code] Display the continuous/on-request checking mode in the status bar,
   allow to change it by clicking on it (@ejgallego, #721)
 - Add an example of multiple workspaces (@ejgallego, @Blaisorblade,
   #611)
 - Don't show types of un-expanded goals. We should add an option for
   this, but we don't have the cycles (@ejgallego, #730, workarounds
   #525 #652)
 - Support for `.lv / .v.tex` TeX files with embedded Coq code
   (@ejgallego, #727)
 - Don't expand bullet goals at previous levels by default
   (@ejgallego, @Alizter, #731 cc #525)
 - [petanque] Return basic goal information after `run_tac`, so we
   avoid a `goals` round-trip for each tactic (@gbdrt, @ejgallego,
   #733)
 - [coq] Add support for reading glob files metadata (@ejgallego,
   #735)
 - [petanque] Return extra premise information: file name, position,
   raw_text, using the above support for reading .glob files
   (@ejgallego, #735)
 - [code] Display server status using the `LanguageStatusItem`
   facility, for now we display version and checking status
   information (moved from #721), and we also allow to toggle the
   checking mode from there (@ejgallego, #728)

# coq-lsp 0.1.8.1: Spring fix
-----------------------------

 - call to VizCar vscode extension, mirroring behavior for
   vizx. (@bhaktishh, #655)

# coq-lsp 0.1.8: Trick-or-treat
-------------------------------

 - Update VSCode client dependencies, should bring some performance
   improvements to goal pretty printing (@ejgallego, #530)
 - Update goal display colors for light mode so they are actually
   readable now. (@bhaktishh, #539, fixes #532)
 - Added link to Python coq-lsp client by Pedro Carrot and Nuno
   Saavedra (@Nfsaavedra, #536)
 - Properly concatenate warnings from _CoqProject (@ejgallego,
   reported by @mituharu, #541, fixes #540)
 - Fix broken `coq/saveVo` and `coq/getDocument` requests due to a
   parsing problem with extra fields in their requests (@ejgallego,
   #547, reported by @Zimmi48)
 - `fcc` now understands the `--coqlib`, `--coqcorelib`,
   `--ocamlpath`, `-Q` and `-R` arguments (@ejgallego, #555)
 - Describe findlib status in `Workspace.describe`, which is printed
   in the output windows (@ejgallego, #556)
 - `coq-lsp` plugin loader will now be strict in case of a plugin
   failure, the previous loose behavior was more convenient for the
   early releases, but it doesn't make sense now and made things
   pretty hard to debug on the Windows installer (@ejgallego, #557)
 - Add pointers to Windows installers (@ejgallego, #559)
 - Recognize `Goal` and `Definition $id : ... .` as proof starters
   (@ejgallego, #561, reported by @Zimmi48, fixes #548)
 - Provide basic notation information on hover. This is intended for
   people to build their own more refined notation feedback systems
   (@ejgallego, #562)
 - Hover request can now be extended by plugins (@ejgallego, #562)
 - Updated LSP and JS client libs, notably to vscode-languageclient 9
   (@ejgallego, #565)
 - Implement a LIFO document scheduler, this is heavier in the
   background as more documents will be checked, but provides a few
   usability improvements (@ejgallego, #566, fixes #563, reported by
   Ali Caglayan)
 - New lexical qed detection error recovery rule; this makes a very
   large usability difference in practice when editing inside proofs.
   (@ejgallego, #567, fixes #33)
 - `coq-lsp` is now supported by the `coq-nix-toolbox` (@Zimmi48,
   @CohenCyril, #572, via
   https://github.com/coq-community/coq-nix-toolbox/pull/164 )
 - Support for `-rifrom` in `_CoqProject` and in command line
   (`--rifrom`). Thanks to Lasse Blaauwbroek for the report.
   (@ejgallego, #581, fixes #579)
 - Export Query Goals API in VSCode client; this way other extensions
   can implement their own commands that query Coq goals (@amblafont,
   @ejgallego, #576, closes #558)
 - New `pretac` field for preprocessing of goals with a tactic using
   speculative execution, this is experimental for now (@amblafont,
   @ejgallego, #573, helps with #558)
 - Implement `textDocument/selectionRange` request, that will return
   the range of the Coq sentence underlying the cursor. In VSCode,
   this is triggered by the "Expand Selection" command. The
   implementation is partial: we only take into account the first
   position, and we only return a single range (Coq sentence) without
   parents. (@ejgallego, #582)
 - Be more robust to mixed-separator windows paths in workspace
   detection (@ejgallego, #583, fixes #569)
 - Adjust printing breaks in error and message panels (@ejgallego,
   @Alizter, #586, fixes #457 , fixes #458 , fixes #571)

# coq-lsp 0.1.7: Just-in-time
-----------------------------

 - New command line compiler `fcc`. `fcc` allows to access most
   features of `coq-lsp` without the need for a command line client,
   and it has been designed to be extensible and machine-friendly
   (@ejgallego, #507, fixes #472)
 - Enable web extension support. For now this will not try to start
   the coq-lsp worker as it is not yet built. (@ejgallego, #430, fixes
   #234)
 - Improvements and clenaups on hover display, in particular we don't
   print repeated `Notation` strings (@ejgallego, #422)
 - Don't fail on missing serlib plugins, they are indeed an
   optimization; this mostly impacts 8.16 by lowering the SerAPI
   requirements (@ejgallego, #421)
 - Fix bug that prevented "Goal after tactic" from working properly
   (@ejgallego, #438, reported by David Ilcinkas)
 - Fix "Error message browser becomes non-visible when there are many
   goals" by using a fixed-position separated error display (@TDiazT,
   #455, fixes #441)
 - Message about workspace detection was printing the wrong file,
   (@ejgallego, #444, reported by Alex Sanchez-Stern)
 - Display the list of pending obligations in info panel (@ejgallego,
   #262)
 - Preliminary support to display document performance data in panel
   (@ejgallego, #181)
 - Fix cases when workspace / root URIs are `null`, as per LSP spec,
   (#453 , reported by orilahav, fixes #283)
 - Pass implicit argument information to hover printer (@ejgallego, #453,
   fixes #448)
 - Fix keybinding for the "Show Goals at Point" command (@4ever2, #460)
 - Alert when `rootPath` is relative (#465, @ejgallego, report by Alex
   Sanchez-Stern)
 - Hook coq-lsp to ViZX extension (@bhaktishh, #469)
 - `proof/goals` request now takes an optional formatting parameter
   so clients can specify it per-request (@ejgallego, @bhaktishh, #470)
 - New command line argument `--idle-delay=$secs` that controls how
   much an idle server will sleep before going back to request
   processing. Default setting is `0.1`, using more aggressive
   settings like `0.01` can decrease latency of requests by ~4x
   (@ejgallego, @hazardouspeach, #467, #471)
 - Warnings from `_CoqProject` files are now applied (@ejgallego,
   reported by @arthuraa, #500)
 - Be more resilient when serializing unknowns Asts (@ejgallego, #503,
   reported by Gijs Pennings)
 - Coq's STM is not linked anymore to `coq-lsp` (@ejgallego, #511)
 - More granular options `send_perf_data` `send_diags`, `verbosity`
   will set them now (@ejgallego, #517)
 - Preliminary plugin API for completion events (@ejgallego, #518,
   fixes #506)
 - Limit the number of messages displayed in the goal window to 101,
   as to workaround slow render of Coq's pretty printing format. This
   is an issue for example in Search where we can get thousand of
   results. We also speed up the rendering a bit by not hashing twice,
   and fix a parameter-passing bug. (@ejgallego, #523, reported by
   Anton Podkopaev)

# coq-lsp 0.1.6: Peek
---------------------

 - The info / goal view now uses jsCoq's client-side rendering, with
   better highlighting and layout rendering (@artagnon, @ejgallego,
   #143, fixes #96)
 - Printing method is now configurable by the user (@ejgallego, #143,
   fixes #321)
 - Trigger completion on quote char "'" (@ejgallego, #350)
 - Fix typo on keybinding config for show goals (@tomtomjhj, #357)
 - New request `coq/getDocument` to get serialized full document
   contents. Thanks to Clément Pit-Claudel for feedback and ideas.
   (@ejgallego, #350)
 - Auto-ignore Coq object files; can be disabled in config
   (@ejgallego, #365)
 - Support workspaces with multiple roots, this is very useful for
   projects that contain several `_CoqProject` files in different
   directories (@ejgallego, #374)
 - Add VS Code commands to start / stop the server (@ejgallego, #377,
   cc #209)
 - Fix bug that made the server not exit on `exit` LSP notification
   (@artagnon, @ejgallego, #375, fixes #230)
 - Lay the foundation for server tests (@artagnon, #356)
 - Remove the `coq-lsp.ok_diagnostics` setting (@artagnon, #129)
 - Print abbreviations on hover (@ejgallego, #384)
 - Print hover types without parenthesis (@ejgallego, #384)
 - Parse identifiers with dot for hover and jump to definition
   (@ejgallego, #385)
 - Update `vscode-languageclient` to 8.1.0 (@ejgallego, @Alizter,
   #383, fixes #273)
 - Fix typo on max_errors checking, this made coq-lsp stop on the
   number of total diagnostics, instead of only errors (@ejgallego,
   #386)
 - Hover symbol information: hypothesis names must shadow globals of
   the same name (@ejgallego, #391, fixes #388)
 - De-schedule document on didClose, otherwise the scheduler will keep
   trying to resume it if it didn't finish (@ejgallego, #392)
 - Hover symbol information: correctly handle identifiers before '.'
   and containing a quote (') themselves (@ejgallego, #393)
 - Add children entries to the table-of-contents (@ejgallego, #394)
 - Invalidate Coq's imperative cache on error (@ejgallego, @r-muhairi,
   #395)
 - Add status bar button to toggle server run status (@ejgallego,
   @Alizter, #378, closes #209)
 - Support for `COQLIB` and `COQCORELIB` environment variables, added
   `--coqcorelib` command line argument (@ejgallego, #403)
 - Protocol infrastructure for code lenses (@ejgallego, #396)
 - Set binary mode for protocol input / output (@ejgallego, #408)
 - Allow to set `ocamlpath` from the command line (@ejgallego, #408)
 - Windows support (@ejgallego, @jim-portegies, #408)
 - Scroll active goal into view (@ejgallego, #410, fixes #381)
 - Server status icon will now react properly to fatal server errors
   (@ejgallego, reported by @Alizter, #411, fixes #399)
 - Info on memory and time is now disabled by default, new option
   `coq-lsp.stats_on_hover_option` to re-enable it (@ejgallego, #412,
   fixes #398).
 - `coq-lsp` can now save `.vo` files for files opened in the
   editor. Use the new "Save to .vo" command, or the new protocol
   `coq/saveVo` request (@ejgallego, #417, fixes #339)

# coq-lsp 0.1.5.1: Path
-----------------------

 - Fix handling of `COQPATH` and `OCAMLPATH` (@ejgallego, #364)

# coq-lsp 0.1.5: Form
---------------------

 - Fix a bug when trying to complete in an empty file (@ejgallego,
   #270)
 - Fix a bug with the position reported by the `$/coq/fileProgress`
   notification (#270)
 - Fix messages panel rendering after the port to React (@ejgallego,
   #272)
 - Fix non-compliance with LSP range type due to extra `offset` field
   (@ejgallego, #271)
 - The goal display now numbers goals starting with 1 instead of 0
   (@artagnon, #277, report by Hugo Herbelin)
 - Markdown Coq code blocks now must specify "coq" as a language
   (@ejgallego, #280)
 - Server is now more strict w.r.t. what URIs it will accept for
   documents, see protocol documentation (@ejgallego, #286, reported
   by Alex Sanchez-Stern)
 - Hypotheses with bodies are now correctly displayed (@ejgallego,
   #296, fixes #293, report by Ali Caglayan)
 - `coq-lsp` incorrectly required the optional `rootPath`
   initialization parameter, moreover it ignored `rootUri` if present
   which goes against the LSP spec (@ejgallego, #295, report by Alex
   Sanchez-Stern)
 - `coq-lsp` will now reject opening multiple files when the
   underlying Coq version is buggy (@ejgallego, fixes #275, fixes
   #281)
 - Fix bug when parsing client option for unicode completion
   (@ejgallego #301)
 - Support unicode characters in filenames (@artagnon, #302)
 - Stop checking documents after a maximum number of errors,
   user-configurable (by default 150) (@ejgallego, #303)
 - Coq Markdown files (.mv extension) are now highlighted properly
   using both Coq and Markdown syntax rules (@4ever2, #307)
 - Goal view now supports find (@Alizter, #309, closes #305)
 - coq-lsp now understands a basic version of Coq Waterproof files
   (.wpn) Note that we don't associate to them by default, as to allow
   the waterproof extension to take over the files (@ejgallego, #306)
 - URI validation is now more strict, and some further bugs should be
   solved; note still this can be an issue on some client settings
   (@ejgallego, #313, fixes #187)
 - Display Coq info and debug messages in info panel (@ejgallego,
   #314, fixes #308)
 - Goal display handles background goals better, showing preview,
   goals stack, and focusing information (@ejgallego, #290, fixes
   #288, fixes #304, based on jsCoq code by Shachar Itzhaky)
 - Warnings are now printed in the info view messages panel
   (@ejgallego, #315, fixes #195)
 - Info protocol messages now have location and level
   (@ejgallego, #315)
 - Warnings are not printed in the info view messages panel
   (@ejgallego, #, fixes #195)
 - Improved `documentSymbol` return type for newer `DocumentSymbol[]`
   hierarchical symbol support. This means that sections and modules
   will now be properly represented, as well as constructors for
   inductive types, projections for records, etc...  (@ejgallego,
   #174, fixes #121, #122)
 - [internal] Error recovery can now execute full Coq commands as to
   amend states, required for #319 (@ejgallego, #320)
 - Auto-admit the previous bullet goal when a new bullet cannot be
   opened due to an unsolved previous bullet. This also works for {}
   focusing operators. This is very useful when navigating bulleted
   proofs (@ejgallego, @Alizter, #319, fixes #300)
 - Store Ast.Info.t incrementally (@ejgallego, #337, fixes #316)
 - Basic jump to definition support; due to lack of workspace
   metadata, this only works inside the same file (@ejgallego, #318)
 - Show type of identifiers at point on hover (@ejgallego, #321, cc:
   #164)

# coq-lsp 0.1.4: View
---------------------

 - Support for OCaml 4.11 (@ejgallego, #184)
 - The keybinding alt+enter in VSCode is now correctly scoped to be
   only active on Coq files (@artagnon, #188)
 - Support Unicode files (@ejgallego, #200, fixes #193, fixes #197)
 - The info / goal view is now script enabled and does client-side
   rendering. It is also now bundled with esbuild as part of the build
   process (@artagnon, @ejgallego, #171)
 - The no-op `--std` argument to the `coq-lsp` binary has been
   removed, beware of your setup in the extension settings
   (@ejgallego, #208)
 - Settings for the VSCode extension are now categorized (@Alizter, #212)
 - `GoalAnswer`s now include the proof "stack" and better hypothesis
   information, changes are compatible with 0.1.3 `GoalAnswer` version
   (@ejgallego, #237)
 - Focus is now preserved when the info view pops up (@artagnon, #242,
   fixes #224)
 - In `_CoqProject`, `-impredicative-set` is now parsed correctly
   (@artagnon, #241)
 - InfoView is not written in React (@ejgallego, #223)
 - `debug` option in the client / protocol that will enable Coq's backtraces
   (@Alizter, @ejgallego, #217, #248)
 - Full document stats are now correctly computed on checking
   resumption, still cached sentences will display the cached timing
   tho (@ejgallego, #257)
 - Set Coq library name correctly from URI, note this makes the server
   to accept less URIs (@ejgallego, #260)
 - `_CoqProject` file is now detected using LSP client `rootPath`
   (@ejgallego, #261)
 - You can press `\` to trigger Unicode completion by the server. This
   behavior is configurable, with "off", "regular", and "extended"
   settings (@artagnon, @Alizter, ejgallego, #219).

# coq-lsp 0.1.3: Event
----------------------

 - Much improved handling of Coq fatal errors, the server is now
   hardened against them (@ejgallego, #155, #157, #160, fixes #91)
 - `coq-lsp` now follows the LSP specification regarding
   initialization strictly (@ejgallego, #168)
 - New setting for goals to be updated when the selection changes due
   to a command; this makes VsCodeVim cursor tracking work; thanks to
   Cactus (Anton) Golov for detailed bug reporting and testing
   (@ejgallego, @jesyspa, #170, fixes #163)
 - `coq-lsp` will now warn the user when two files have been opened
   simultaneously and the parser may go into a broken state :/
   (@ejgallego, #169)
 - Implement request postponement and cancellation. Thus
   `documentSymbols` will now be postponed until the document is
   ready, (@ejgallego, #141, #146, fixes #124)
 - Protocol and VS Code interfaces now support shelved and given_up
   goals (@ejgallego, #175)
 - Allow to postpone requests to wait for data to become available on
   document points; this is implemented to provide a nicer "show goals
   while I type" experience. Client default has been changed to "show
   goals on mouse, click, typing, and cursor movement) (@ejgallego,
   #177, #179)
 - Store stats per document (@ejgallego, #180, fixes #173)

# coq-lsp 0.1.2: Message
------------------------

 - Send an error to the client if the client and server versions don't
   match (@ejgallego, #126)
 - Parse options -noinit, -indices-matter, and -impredicative-set from
   `_CoqProject` (@artagnon, @ejgallego, #140, #150)
 - Log file `log-lsp.txt` has been removed in favor of `coq-lsp.trace.server`
   (@artagnon, @ejgallego, #130, #148)
 - Added --bt flag to print a backtrace on error (@Alizter, #147)
 - A detailed view of Coq errors is now displayed in the info panel
   (@ejgallego, #128)
 - Coq "Notice" messages, such as the ones generated by `About` or
   `Search` are not shown anymore as diagnostics. Instead, they will
   be shown on the side panel when clicking on the corresponding
   command. The `show_notices_as_diagnostics` option allows to restore
   old behavior (@ejgallego, #128, fixes #125)
 - Print some more info about Coq workspace configuration (@ejgallego, #151)
 - Admit failed `Qed` by default; allow users to configure it
   (@ejgallego, #118, fixes #90)

# coq-lsp 0.1.1: Location
-------------------------

 - Don't crash if the log file can't be created (@ejgallego, #87)
 - Use LSP functions for client-side logging (@ejgallego, #87)
 - Log `_CoqProject` detection settings to client window (@ejgallego, #88)
 - Use plugin include paths from `_CoqProject` (@ejgallego, #88)
 - Support OCaml >= 4.12 (@ejgallego, #93)
 - Optimize the number of diagnostics sent in eager mode (@ejgallego, #104)
 - Improved syntax highlighting on VSCode client (@artagnon, #105)
 - Resume document checking from the point it was interrupted
   (@ejgallego, #95, #99)
 - Don't convert Coq "Info" messages such as "Foo is defined" to
   feedback by default; users willing to see them can set the
   corresponding option (@ejgallego, #113)
 - Send `$/coq/fileProgress` progress notifications from server,
   similarly to what Lean does; display them in Code's right gutter
   (@ejgallego, #106, fixes #54)
 - Show goals on click by default, allow users to configure the
   behavior to follow cursor in different ways (@ejgallego, #116,
   fixes #89)
 - Show file position in goal buffer, use collapsible elements for
   goal list (@ejgallego, #115, fixes #109)
 - Resume checking from common prefix on document update (@ejgallego,
   #111, fixes #110)
 - Only serve goals, hover, and symbols requests when the document
   has been sufficiently processed (@ejgallego, #120, fixes #100)

# coq-lsp 0.1.0: Memory
-----------------------

 - Location-aware cache for incremental Coq interpretation (@ejgallego)
 - Smart, structure-aware error recovery (@ejgallego)
 - Configure flags reading _CoqProject file (@artagnon, #3)
 - Interruption support (@ejgallego , @Alizter, #27, #32, #34)
 - Markdown support (@ejgallego, #62)
 - Goal display (@ejgallego @corwin-of-amber, #69)
 - User-side configuration (@ejgallego, #67)
 - Allow to configure before/after goal display (@ejgallego, #78)
 - Allow requests to interrupt checking (@ejgallego, #76)

# coq-lsp 0.0.0.1
-----------------

 - Bootstrap from lambdapi-lsp server (@ejgallego)
