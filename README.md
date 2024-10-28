# Programa de Processamento de Cartas Pokémon

---

## Identificação
**Nome:** Antônio Sérgio Rodrigues Tolio  
**Curso:** Sistemas de Informação  

---

## Visão Geral
Este projeto processa dados de cartas Pokémon usando um programa Haskell e um script Python. Esses componentes trabalham juntos para ler dados de entrada, consultar uma API para obter detalhes das cartas, formatar as informações e gerar um arquivo final com os dados processados, que pode ser utilizado no site de compras de cartas [ligapokemon.com.br](https://ligapokemon.com.br).

---

## Componentes do Projeto

### Programa Haskell
O programa Haskell executa as seguintes tarefas:

1. **Leitura e Parseamento da Entrada:**  
   A função `parseCards` lê a entrada de texto, divide em linhas e extrai informações como quantidade, nome e número da carta.

2. **Consulta à API:**  
   A função `fetchCard` utiliza `makeGetRequest` do módulo `PokemonApiScrapper` para consultar uma API externa com o nome e número da carta.

3. **Formatação e Salvamento dos Resultados:**  
   - `formatCardResponse` formata a resposta JSON da API.
   - `saveResultsToTxt` salva os dados formatados no arquivo `cards.txt` no seguinte formato:
     ```
     quantidade: quantidade do card, nameCorreto: nome do card, numeroCorreto: numero do card, json: resposta JSON da chamada
     ```

#### Principais Desafios
Durante o desenvolvimento, encontrei dificuldades com a decodificação do JSON retornado pela função `makeGetRequest`, devido a problemas com a biblioteca Wreq e as palavras reservadas do Haskell. Após muitas tentativas, resolvi o problema usando a biblioteca Aeson para decodificação. Algumas das tentativas de solução estão descritas abaixo.

**Exemplo de Tentativa de Decodificação:**
   ```
formatCardResponse card jsonResponse =
  case decode (B.pack $ map (fromIntegral . fromEnum) jsonResponse) :: Maybe Value of
    Just val -> case parseMaybe (withObject "root" $ \v -> do
                                  dataArr <- v .: "data"
                                  let firstCard = head (dataArr :: [Value])
                                  withObject "cardData" (\o -> do
                                    set <- o .: "set"
                                    total <- set .: "total"
                                    return $ "Quantidade: " ++ show (quantity card) ++
                                             ", Nome do Card: " ++ name card ++
                                             ", Número do Card: " ++ show (cardNumber card) ++
                                             ", Total de Sets: " ++ show (total :: Int)) firstCard) val of
      Just result -> Right result
      Nothing -> Left $ "Falha ao parsear JSON: " ++ jsonResponse
    Nothing -> Left $ "Falha ao decodificar JSON: " ++ jsonResponse
   ```

#### Principais Funções do Programa Haskell:
- `parseCards`: Converte a entrada de texto em uma lista de cartas.
- `fetchCard`: Consulta a API para obter detalhes da carta.
- `formatCardResponse`: Formata a resposta da API.
- `saveResultsToTxt`: Salva os resultados em `cards.txt`.

### Script Python
Foi necessário criar um script Python para processar os dados de cartas Pokémon. O script lê o arquivo `cards.txt`, filtra e formata as informações e salva os resultados em `formatted_cards.txt`. De maneira que não consegui decodificar o JSON retornado pela API no programa Haskell, o script Python foi criado para realizar essa tarefa.

O script Python realiza as seguintes operações:

1. **Leitura do Arquivo:**  
   A função `process_file` lê o arquivo `cards.txt` linha por linha.

2. **Filtragem e Formatação dos Dados:**  
   - A função `extract_info` extrai detalhes como quantidade, `nameCorreto`, `numeroCorreto` e o total do objeto JSON dentro de `set`.
   - Ajusta o formato de `numeroCorreto` para três dígitos.

3. **Salvamento dos Resultados Formatados:**  
   - A função `save_results` salva os dados em `formatted_cards.txt`.

#### Principais Funções do Script Python:
- `extract_info`: Extrai informações relevantes de cada linha do arquivo.
- `process_file`: Processa as linhas do arquivo para extrair as informações.
- `save_results`: Salva os resultados formatados em `formatted_cards.txt`.

---

## Como Executar

1. **Execute o Programa Haskell:**  
   Compile e execute o programa Haskell para gerar o arquivo `cards.txt`.
   ```
   stack build
   stack run
   ```
   Caso esteja rodando no Codespace (o que eu não recomendo pois ele tem que instalar muitas dependências), execute o comando abaixo:
   ```
   stack setup --install-ghc
   stack build --allow-different-user
   stack run --allow-different-user
   ```

2. **Preencha o Arquivo `cards.txt` com Dados da API:**  
   Faça uma chamada para a URL de cards da API para que o arquivo `cards.txt` seja preenchido com os dados corretos.
   ```
   curl --location 'http://localhost:3000/cards?data=Pok%C3%A9mon%3A%2020%0A4%20Archeops%20SIT%20147%0A3%20Lugia%20V%20SIT%20138%0A3%20Lugia%20VSTAR%20SIT%20139%0A3%20Minccino%20TEF%20136%0A3%20Cinccino%20TEF%20137%0A2%20Lumineon%20V%20BRS%2040%0A%0A' \
   --form 'data="Pokémon: 18
   3 Origin Forme Palkia V ASR 39
   2 Origin Forme Palkia VSTAR ASR 40
   3 Duskull BRS 60
   1 Dusclops SFA 19
   2 Dusknoir SFA 20
   1 Froakie OBF 56
   1 Greninja ex TWM 106
   1 Radiant Greninja ASR 46
   1 Bloodmoon Ursaluna ex TWM 141
   1 Squawkabilly ex PAL 169
   1 Fezandipiti ex SFA 38
   1 Mew ex MEW 151

   Trainer: 35
   4 Iono PAL 185
   2 Irida ASR 147
   4 Nest Ball SVI 181
   4 Ultra Ball SVI 196
   4 Rare Candy SVI 191
   4 Night Stretcher SFA 61
   2 Trekking Shoes ASR 156
   2 Earthen Vessel PAR 163
   1 Buddy-Buddy Poffin TEF 144
   1 Hisuian Heavy Ball ASR 146
   1 Counter Catcher PAR 160
   1 Prime Catcher TEF 157
   1 Switch SVI 194
   1 Lost Vacuum LOR 162
   3 PokéStop PGO 68

   Energy: 7
   7 Water Energy SVE 11"'

   ```

3. **Execute o Script Python:**  
   Execute o script Python `process_json.py` para ler `cards.txt`, filtrar e formatar os dados, e salvar os resultados em `formatted_cards.txt`.
   ```
   python3 process_json.py
   ```


## Exemplo de Saída

### `cards.txt`
```
1: quantidade:3, nameCorreto:Bloodmoon Ursaluna ex, numeroCorreto:141, json:...
2: quantidade:2, nameCorreto:Origin Forme Palkia V, numeroCorreto:39, json:...
```

### `formatted_cards.txt`
```
1: 3 Bloodmoon Ursaluna ex (141/167)
2: 2 Origin Forme Palkia V (039/216)
```

---

## Referências e Créditos

- **Referências**:  

  - [Aeson: Parsing JSON in Haskell](https://artyom.me/aeson)
  - [Wreq Tutorial](http://www.serpentine.com/wreq/tutorial.html)
  - [Scotty: REST API in Haskell](https://hackage.haskell.org/package/scotty)
  - [Getting Started with Haskell Projects](https://www.stackbuilders.com/insights/getting-started-with-haskell-projects-using-scotty)
  - [Stack](https://docs.haskellstack.org/en/stable/README/)

Link do video: https://youtu.be/eLurf0PXIFA