-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the APpy APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.
import Definitions
import Parser
import Transformer

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Smoke tests" [
  testGroup "Parser" [
    testCase "Very simple term" $
      parseSpec "---\n S {:stmt}::= ab {abc}." @?=
        Right ("",[(("S", RPlain, Just (AUser "stmt")), ESeq [ESimple (SNTerm "ab")] "abc")]),
    testCase "Very simple term2" $
      parseSpec "---\n S {:stmt}::= aa." @?=
        Right ("",[(("S",RPlain,Just (AUser "stmt")), ESimple (SNTerm "aa"))]),
    testCase "Very simple lit" $
      parseSpec "---\n S ::= \"a\"{abc}." @?=
        Right ("",[(("S",RPlain,Nothing), ESeq [ESimple (SLit "a")] "abc")]),
    testCase "Very simple lit error" $
      parseSpec "---\n S {:stmts}::= \"a\" {abc}." @?=
        Right ("",[(("S", RPlain, Just (AUser "stmts")), ESeq [ESimple (SLit "a")] "abc")]),
    testCase "Very simple charlit" $
      parseSpec "---\n S ::= 'a'{abc}." @?=
        Right ("",[(("S",RPlain,Nothing), ESeq [ESimple (SChar 'a')] "abc")]),
    testCase "Very simple anychar" $
      parseSpec "---\n S ::= \"@\"  {abc}." @?=
        Right ("",[(("S",RPlain,Nothing), ESeq [ESimple SAnyChar] "abc")]),
    testCase "Very simple bar" $
      parseSpec "---\n S ::=b{cbd}|a{abc}|\"@\"." @?=
        Right ("",[(("S",RPlain,Nothing),EBar (EBar (ESeq [ESimple (SNTerm "b")] "cbd") (ESeq [ESimple (SNTerm "a")] "abc")) (ESimple SAnyChar))]),
    testCase "Very simple ?" $
      parseSpec "---\nnum ::= '-'? Digit{maybe _2 (\\_ -> negate _2) _1}." @?=
        Right ("",[(("num",RToken,Nothing),ESeq [EOption (ESimple (SChar '-')),ESimple (SNTerm "Digit")] "maybe _2 (\\_ -> negate _2) _1")]),
    testCase "Very simple *" $
      parseSpec "---\n digit2 ::= Digit Digit* Digit?{_1 _2 _3}." @?=
        Right ("",[(("digit2",RToken,Nothing),ESeq [ESimple (SNTerm "Digit"),EMany (ESimple (SNTerm "Digit")),EOption (ESimple (SNTerm "Digit"))] "_1 _2 _3")]),
    testCase "Very simple !" $
      parseSpec "---\n digit ::= !Digit{Not _2}." @?=
        Right ("",[(("digit",RToken,Nothing),ESeq [ENot (ESimple (SNTerm "Digit"))] "Not _2")]),
    testCase "Very simple RSep !" $
      parseSpec "---\n _digit ::= !Digit{Not _2}." @?=
        Right ("",[(("_digit",RSep,Nothing),ESeq [ENot (ESimple (SNTerm "Digit"))] "Not _2")]),
    testCase "(alt)" $
      parseSpec "---\ndigit ::= c|(b f{a}|a d{f})." @?=
        Right ("",[(("digit",RToken,Nothing),EBar (ESimple (SNTerm "c"))
          (EBar 
            (ESeq [ESimple (SNTerm "b"),ESimple (SNTerm "f")] "a")
            (ESeq [ESimple (SNTerm "a"),ESimple (SNTerm "d")] "f")
          ))]),
    testCase "simple error not 3 dashes" $
      parseSpec "--\n-\n digit ::= Digit." @?=
        Left "\"Error\" (line 1, column 1):\nunexpected \"\\n\"\nexpecting \"---\"",
    testCase "simple error no dot" $
      parseSpec "---\n digit ::= Digit" @?=
        Left "\"Error\" (line 2, column 17):\nunexpected end of input\nexpecting \"{?\", \"?\", \"*\", tab, new-line, space, white space, \"|\" or \".\"",
    testCase "simple comment (not implemented)" $ -- failing as its not implemented
      parseSpec "---\n digit ::= Digit--123" @?=
         Right ("",[(("digit",RToken,Nothing), ESimple (SNTerm "Digit"))]),
    testCase "simple multiline" $
      parseSpec "let a = 'a'\n---\n digit ::= !Digit{Not _2}.\ndigit2 ::= Digit Digit* Digit?{_1 _2 _3}." @?=
        Right ("let a = 'a'\n",[(("digit",RToken,Nothing),ESeq [ENot (ESimple (SNTerm "Digit"))] "Not _2"),(("digit2",RToken,Nothing),ESeq [ESimple (SNTerm "Digit"),EMany (ESimple (SNTerm "Digit")),EOption (ESimple (SNTerm "Digit"))] "_1 _2 _3")])
  ],
  testGroup "converter" [
    testCase "convert simple" $
      convert [(("digit",RToken,Nothing),ESimple (SNTerm "c"))] @?=
        Right [(("digit",RToken,Nothing),[([SNTerm "c"],AUser "")])],
    testCase "convert Seq" $
      convert [(("digit",RToken,Nothing),ESeq [ESimple (SNTerm "c"), ESimple (SLit "+")] "_1 +")] @?=
        Right [(("digit",RToken,Nothing),[([SNTerm "c", SLit "+"],AUser "_1 +")])],
    testCase "convert nested bar" $
      convert [(("digit",RToken,Nothing),(ESeq [EBar (ESimple (SNTerm "c")) (ESimple (SNTerm "b"))] "abc"))] @?=
        Right ([
        (
            ("digit",RToken,Nothing),
            [([SNTerm "digit'"],AVar "digit'")]
        ),
        (
            ("digit'",RPlain,Nothing),
            [
                ([SNTerm "c"],AUser "abc"),
                ([SNTerm "b"],AUser "abc")
            ]
        )
    ]),
    testCase "convert multiple nested bar" $
      convert [(("digit",RToken,Nothing),
        EBar 
          (ESimple (SNTerm "a"))
          (EBar
            (ESimple (SNTerm "b"))
            (EBar 
              (ESimple (SNTerm "c"))
              (EBar 
                (ESimple (SNTerm "d"))
                (ESimple (SNTerm "e"))))))] @?=
        Right [
          (("digit",RToken,Nothing),[([SNTerm "a"],AUser ""),([SNTerm "digit'"],AVar "digit'")]),
          (("digit'",RPlain,Nothing),[([SNTerm "b"],AUser ""),([SNTerm "c"],AUser ""),([SNTerm "d"],AUser "")])
          ],
    testCase "convert nested bar advanced" $
      convert [(("digit",RToken,Nothing),
        ESeq [
            EBar (ESimple (SNTerm "c")) (ESimple (SNTerm "b")),
            EBar (ESimple (SNTerm "d")) (ESimple (SNTerm "e"))
            ] "abc")] @?=
        Right [(("digit",RToken,Nothing),
        [([SNTerm "digit'",SNTerm "digitb'"],AVar "digit'")]),
        (("digit'",RPlain,Nothing),[([SNTerm "c"],AUser "abc"),
        ([SNTerm "b"],AUser "abc")]),(("digitb'",RPlain,Nothing),
        [([SNTerm "d"],AUser "abc"),([SNTerm "e"],AUser "abc")])],
    testCase "convert option seq" $
      convert [(("digit",RToken,Nothing),(ESeq [ESimple (SNTerm "a"), EOption (ESimple (SLit "?"))] "123"))] @?=
        Right [
          (
            ("digit",RToken,Nothing),
            [([SNTerm "a", SNTerm "digit'opt"],AUser "123")]
          ),
          (
            ("digit'opt",RPlain,Nothing),
            [
              ([SLit "?"],AUser "123"),
              ([],AUser "Nothing")
            ]
          )
        ],
    testCase "convert repeat" $
      convert [(("digit",RToken,Nothing),EMany (ESimple (SNTerm "a")))] @?=
        Right [
          (
            ("digit",RToken,Nothing),
            [
              (
                [
                  SNTerm "digit'm"
                ],
                AVar "digit'm"
              )
            ]
          ),
          (
            ("digit'm",RPlain,Nothing),
            [
              (
                [
                  SNTerm "a",
                  SNTerm "digit'm"
                ],
                AUser ""
              ),
              (
                [],
                AUser "Nothing"
              )
            ]
          )
        ],
    testCase "convert repeat advanced" $
      convert [(("digit",RToken,Nothing),ESeq [ESimple (SNTerm "ab"), EMany (ESeq [ESimple (SNTerm "a"), (ESimple (SLit "?"))] "123"), ESimple (SNTerm "BA")] "123")] @?=
        Right [
          (
            ("digit",RToken,Nothing),
            [
              (
                [
                  SNTerm "ab",
                  SNTerm "digit'm",
                  SNTerm "BA"
                ],
                AUser "123"
              )
            ]
          ),
          (
            ("digit'm",RPlain,Nothing),
            [
              (
                [
                  SNTerm "a",
                  SLit "?",
                  SNTerm "digit'm"
                ],
                AUser "123"
              ),
              (
                [],
                AUser "Nothing"
              )
            ]
          )
        ],
    testCase "convert not" $
      convert [(("digit",RToken,Nothing),ENot (ESeq [ESimple (SNTerm "ab"), ESimple (SNTerm "jjj")] "123"))] @?=
        Right [(("digit",RToken,Nothing),[([SNot (SNTerm "ab"),SNot (SNTerm "jjj")],AVar "digit'm")])],

    testCase "multiline" $
      convert [(("digit",RToken,Nothing), EBar (ESimple (SNTerm "ab")) (ESimple (SNTerm "jj"))),
      (("ab",RToken,Nothing), ESimple (SNTerm "ab")),
      (("jjj",RToken,Nothing), ESimple (SNTerm "ab"))
      ] @?=
        Right [(("digit",RToken,Nothing),[([SNTerm "ab"],AUser ""),([SNTerm "jj"],AUser "")]),(("ab",RToken,Nothing),[([SNTerm "ab"],AUser "")]),(("jjj",RToken,Nothing),[([SNTerm "ab"],AUser "")])]
  ]]
