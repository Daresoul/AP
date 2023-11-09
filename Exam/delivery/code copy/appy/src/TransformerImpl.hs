-- Put yor transformer implementation in this file
module TransformerImpl where

import Definitions


-- used to convert from EBNF (EGrammar) to BNF (Grammar) should in the future implement simple checks on the grammar also.
convert :: EGrammar -> EM Grammar
convert [] = return []
convert eGrammar =
    case eGrammar of
    x:xs -> case x of
        -- creating our current rules and all the new rules that is found doing the conversion.
        ((name, kind, t), erhs) -> do  let (a,b) = createRule erhs name "" in (do  ys <- convert xs; return (([((name, kind, t), b)] ++ (a)) ++ ys))
                            
createRule :: ERHS -> String -> String -> ([Rule], [([Simple], Action)])
createRule erhs name htext =
        case erhs of
            -- since we are sending another eseq all the time we gotta catch when there is none left
            ESeq [] text -> ([], [])
                                -- calculating the nested conversion
            ESeq (x:xs) text -> case nested x name text of
                                        -- sending back to find the next on from the ESeq the same way as above
                (newRule, oldRule) -> case createRule (ESeq xs text) (name) text of
                                                -- adding together the current rule we are building
                    (newRule2, oldRule2) -> case oldRule2 ++ oldRule of
                        [] -> (newRule ,[([], AUser text)])
                        y:ys ->
                            -- adding together the rules
                            ((newRule ++ newRule2),
                            [
                                -- this just adds together all of the simples into an array on the left hand side of the tuple
                                foldr
                                    (\ (sa, a) (sa2, a2) ->
                                        ((++) sa sa2, a)
                                    ) y ys
                            ])
            EBar erhs1 erhs2 ->
                    -- everything after here would be nested and therefore call each side of the bar as nested
                case nested erhs1 name htext of
                    (newRules, oldRule) ->
                        case nested erhs2 name htext of
                        -- just adding all new rules from each side as well as the left sides together
                            (newRules2, oldRule2) -> (newRules ++ newRules2, oldRule ++ oldRule2)
            EOption erhs2 -> eoptionCase erhs2 name htext
            EMany erhs2 -> emanyCase erhs2 name htext
            ENot erhs2 -> enotCase erhs2 name htext
            -- if its a simple just simply add it
            ESimple s -> ([], [([s], AUser htext)])

-- works mostly like the one above but takes hand of things when they are nested.
nested :: ERHS -> String -> String -> ([Rule], [([Simple], Action)])
nested erhs name htext =
    case erhs of
        ESeq [] text -> ([], [])
        ESeq (x:xs) text -> case nested x name text of
            (newRule, oldRule) -> case nested (ESeq xs text) name text of
                (newRule2, oldRule2) -> case oldRule2 ++ oldRule of
                    [] -> (newRule ,[([], AUser text)])
                    y:ys ->
                        ((newRule ++ newRule2),
                        [
                            -- same as in create rule
                            foldr
                                (\ (sa, a) (sa2, a2) ->
                                    ((++) sa sa2, a)
                                ) y ys
                        ])
        EBar erhs1 erhs2 ->
            case createRule erhs1 name htext of
                (newRules, oldRule) ->
                    case createRule erhs2 name htext of
                        (newRules2, oldRule2) ->
                            (
                                -- if an Ebar is used here a new rules should be made so we add all new ones each side creates
                                -- and creates a new one for this
                                (newRules ++ newRules2) ++
                                [
                                    ((mark_name name, RPlain, Nothing), oldRule ++ oldRule2)
                                ],
                                [
                                    ([(SNTerm (mark_name name))], AVar (mark_name name))    
                                ]
                            )
        EOption erhs2 -> eoptionCase erhs2 name htext
        EMany erhs2 -> emanyCase erhs2 name htext
        ENot erhs2 -> enotCase erhs2 name htext
        ESimple s -> ([], [([s], AUser htext)])
        a -> error (show a)

-- creates the rules for what to do if there exist an option in the EGrammar
eoptionCase :: ERHS -> String -> String -> ([Rule], [([Simple], Action)])
eoptionCase erhs name htext =
    case createRule erhs name htext of
        (newRules, oldRule) ->
            (
                (
                    -- adding the new rules calculated from the ERHS
                    newRules ++
                    [
                        -- The new rule created is the already created old rule but with a nothing
                        ((opt_name name, RPlain, Nothing), (oldRule ++ [([], AUser "Nothing")]))
                    ]
                ),
                [
                    -- here we should just add the new stuff that is necesarry
                    ([SNTerm (opt_name name)], AVar (opt_name name))
                ]
            ) 

-- creates the rules for what to do if there exist an EMany in the EGrammar
emanyCase :: ERHS -> String -> String -> ([Rule], [([Simple], Action)])
emanyCase erhs name htext =
    case nested erhs name htext of
        (newRules, oldRule) ->
            case oldRule of
                x:xs -> -- Always consists of one element, from how it is build
                    case x of
                        (simples, action) ->
                            (
                                (
                                    newRules ++
                                    [
                                        -- creating a new rule, using the simples from the old rule and the action with the new one and an empty one
                                        ((many_name name, RPlain, Nothing),  [(simples ++ [SNTerm (many_name name)], action), ([], AUser "Nothing")])
                                    ]
                                ),
                                [
                                    -- just creating a simple call towards them
                                    ([SNTerm (many_name name)], AVar (many_name name))
                                ]
                            )
-- creates the rules for what to do if there exist an ENot in the EGrammar
enotCase :: ERHS -> String -> String -> ([Rule], [([Simple], Action)])          
enotCase erhs name htext =
    case nested erhs name htext of
        (newRules, oldRule) ->
            case oldRule of
                x:xs -> -- Always consists of one element, from how it is build
                    case x of
                        (simples, action) ->
                            (
                                (
                                    newRules
                                ),
                                [
                                    (setSNotOnArray simples, AVar (many_name name))
                                ]
                            )

-- set SNot infron of all the simples given as input
setSNotOnArray :: [Simple] -> [Simple]
setSNotOnArray [] = []
setSNotOnArray (s:sx) = [SNot s] ++ (setSNotOnArray sx)

-- adds 'opt to a given string
opt_name :: String -> String
opt_name s = s ++ "'opt"

-- adds 'm to a given string
many_name :: String -> String
many_name s = s ++ "'m"

-- adds ' to a given string
mark_name :: String -> String
mark_name s = s ++ "'"

-- (RLHS, [([Simple]{-seq-}, Action)]{-alts-})
-- convert [(("_", RPlain, Nothing ), ESimple (SLit "123"))]
-- convert [(("_", RPlain, Nothing ), ESeq [ESimple (SLit "123")] "123")]
-- convert [(("_", RPlain, Nothing ), ESeq [ESimple (SLit "123"), ESimple (SLit "321")] "123")]

{--
Supposed to be removing lre is not doing it correctly and creates a mess and is not working at all......
--}
lre :: Grammar -> EM Grammar
lre [] = return []
lre grammar =
    case grammar of
        x:xs ->
            case x of
                ((name, kind, t), xs) ->
                    case lreHelper name kind t xs of  -- gets new rules as well as how to build this rule
                        (rules, rhs) ->
                            return ([((name, kind, t), rhs)] ++ rules)

-- gonna go over each element of the array to check for left recursion
lreHelper :: String -> RKind -> Maybe Type -> [([Simple], Action)] -> ([Rule], [([Simple], Action)])
lreHelper name kind t rhs =
    case rhs of
        x:xs ->
            case lreHelper2 name kind t x of
                (rules, simples, simple, action) -> 
                    ((rules, (lreAdd [(simples, action)] simple) ++ [([simple], action)])) 

-- Gonna use an element to eliminate the left recursion
lreHelper2 :: String -> RKind -> Maybe Type -> ([Simple],Action) -> ([Rule], [Simple], Simple, Action)
lreHelper2 name kind t elem =
    case elem of
        ((x:xs), action) ->
            case x of
                SNTerm name ->
                    (
                        [
                            -- A'
                            ((name, kind, t), [((xs ++ [SNTerm (name ++ "'v")]), action), ([], AUser "Nothing")])
                        ],
                        xs,
                        SNTerm (name ++ "'v"),
                        action
                    )

-- adds a simple to each array of [Simple] in [([Simple], Action)] 
lreAdd :: [([Simple], Action)] -> Simple -> [([Simple], Action)]
lreAdd [] s = []
lreAdd (x:xs) s =
    case x of
        (ys, action) ->
            if length ys > 0    then [(ys ++ [s], action)] ++ (lreAdd xs s)
                                else [([], action)] ++ (lreAdd xs s)

{--
Not implemented will just return the same grammar as is given.
--}
lfactor :: Grammar -> EM Grammar
lfactor grammar = return grammar


{--
[
    (
        ("digit",RToken,Nothing),
        [
            (
                [
                    SNTerm "a"
                ],
                AUser ""
            ),
            (
                [
                    SNTerm "digit'"
                ],
                AVar "digit'"
            )
        ]
    ),
    (
        ("digit'",RPlain,Nothing),
        [
            (
                [
                    SNTerm "b"
                ],
                AUser ""
            ),
            (
                [
                    SNTerm "c"
                ],
                AUser ""
            ),
            (
                [
                    SNTerm "d"
                ],
                AUser ""
            )
        ]
    )
]



[
    (
        ("digit",RToken,Nothing),
        [
            (
                [
                    SNTerm "digit'",
                    SNTerm "digit'"
                ],
                AVar "digit'"
            )
        ]
    ),
    (
        ("digit'",RPlain,Nothing),
        [
            (
                [
                    SNTerm "c"
                ],
                AUser "abc"
            ),
            (
                [
                    SNTerm "b"
                ],
                AUser "abc"
            )
        ]
    ),
    (
        ("digit'",RPlain,Nothing),
        [
            (
                [
                    SNTerm "d"
                ],
                AUser "abc"
            ),
            (
                [
                    SNTerm "e"
                ],
                AUser "abc"
            )
        ]
    )
]



[
    (
        ("digit",RToken,Nothing),
        [
            ([],AUser ""),
            (
                [
                    SNTerm "f",
                    SNTerm "c'v"
                ],
                AUser ""
            )
        ]
    ),
    (
        ("c",RToken,Nothing),
        [
            (
                [
                    SNTerm "f",
                    SNTerm "c'v"
                ],
                AUser ""
            ),
            (
                [],
                AUser "Nothing"
            )
        ]
    )
]
--}