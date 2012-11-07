{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

free (Var v) = [ v ]
free (Lam v t) = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

subst :: Term -> Variable -> Term -> Term --what= на ЧТО мы заменяем, var - что
subst t@(Var v) var what = if v == var then what else t
subst t@(Lam v b) var what = if v == var then t else Lam v (subst b var what)
subst (App t t') var what = App (subst t var what) (subst t' var what)

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

betaReduction :: Term -> Variable -> Term -> Term
betaReduction term var what = subst (rename (free what) term) var what
  where rename vars body = case body of
          Var _ -> body
          Lam v t -> Lam v' t'
            where 
		v' = if v `elem` vars then newname (vars ++ free t) v else v
		t' = if v `elem` vars then subst t v (Var v') else t
	  App t t' -> App (rename vars t) (rename vars t')

hasRedex :: Term -> Bool
hasRedex term = case term of
	Var _ -> False
	App (Lam _ t) t' -> True
	Lam _ t -> hasRedex t
	App t t' -> (hasRedex t) || (hasRedex t')



------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

wh, no, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком - редукция к самому левому из самых внутренних
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = sa (n-1) (applRed t)		

applRed term = case term of
	Var _ -> term
	Lam var body -> Lam var (applRed body)
	App (Lam var body) term'-> betaReduction body var term'
	App term term' -> if (hasRedex term') then (App term (applRed term')) else (App (applRed term) term')	    
	

-- Нормализация нормальным порядком - редукция к самому левому из самых внешних
 
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = no (n-1) (normRed t)

normRed term = case term of
	Var _ -> term
	Lam var body-> Lam var (normRed body)
	App (Lam var body) term' -> betaReduction body var term' 
	App term term' -> if (hasRedex term) then (App (normRed term) term') else (App term (normRed term'))

-- Редукция в слабую головную нормальную форму - нпр без альфа-конверсий
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = wh (n - 1) (weakHeadRed t)

weakHeadRed term = case term of
	App (Lam var body) term' -> subst body var term'
	App term term' -> App (weakHeadRed term) term'
	_ -> term

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
--wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).