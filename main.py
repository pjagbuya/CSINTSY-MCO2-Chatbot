import re, os
class c:
   PURPLE = '\033[95m'
   CYAN = '\033[96m'
   DARKCYAN = '\033[36m'
   BLUE = '\033[94m'
   GREEN = '\033[92m'
   YELLOW = '\033[93m'
   RED = '\033[91m'
   BOLD = '\033[1m'
   UNDERLINE = '\033[4m'
   END = '\033[0m'

from pyswip import Prolog

#for sentence pattern relationship
singular_rel = ["sibling", "brother", "sister",
                "father", "mother", "parent",
                "grandmother", "grandfather", "child",
                "daughter", "son", "uncle", "aunt", 
                "relatives", "children", "parents"]

# Regex Pattern for Questions
yesNoQuestions = [  r'Are (.+) and (.+) siblings\?',                r'Is (.+) a sister of (.+)\?',
                    r'Is (.+) a brother of (.+)\?',                 r'Is (.+) the mother of (.+)\?',
                    r'Is (.+) the father of (.+)\?',                r'Are (.+) and (.+) the parents of (.+)\?',
                    r'Is (.+) a grandmother of (.+)\?',             r'Is (.+) a daughter of (.+)\?',
                    r'Is (.+) a son of (.+)\?',                     r'Is (.+) a child of (.+)\?',
                    r'Are (.+) and (.+) children of (.+)\?',        r'Is (.+) an uncle of (.+)\?',
                    r'Is (.+) an aunt of (.+)\?',                   r'Are (.+) and (.+) relatives\?',
                    r'Is (.+) a grandfather of (.+)\?']

whoQuestions = [
    r'Who are the siblings of (.+)\?',
    r'Who are the sisters of (.+)\?',
    r'Who are the brothers of (.+)\?',
    r'Who is the mother of (.+)\?',
    r'Who is the father of (.+)\?',
    r'Who are the parents of (.+)\?',
    r'Who are the daughters of (.+)\?',
    r'Who are the sons of (.+)\?',
    r'Who are the children of (.+)\?'
]

# Regex Pattern for Statements
factStatements = {
    r'(.+) and (.+) are siblings\.',        r'(.+) is a brother of (.+)\.',                   #1
    r'(.+) is a sister of (.+)\.',          r'(.+) is the father of (.+)\.',                  #2
    r'(.+) is the mother of (.+)\.',        r'(.+) and (.+) are the parents of (.+)\.',         #3
    r'(.+) is a grandmother of (.+)\.',     r'(.+) is a grandfather of (.+)\.',               #4
    r'(.+) is a child of (.+)\.',           r'(.+) and (.+) are children of (.+)\.',    #5
    r'(.+) is a daughter of (.+)\.',        r'(.+) is a son of (.+)\.',                       #6
    r'(.+) is an uncle of (.+)\.',          r'(.+) is an aunt of (.+)\.'                      #7
}

pluralToSingle = {"children": "child",
                  "parents": "parent",
                  "relatives": "relatives"}
def initTables():
    # result = bool(list(prolog.query("reset_tables")))
    pass

def printBotHeader(quote):
    print(f"$ {c.BOLD}{c.GREEN}CHATBOT{c.END} > {quote}")
def printResults(results, person, rel):
    results.sort()
    isMany = len(results) > 1

    if not isMany and rel in pluralToSingle.keys():
        rel = pluralToSingle[rel]

    if isMany and rel in pluralToSingle.values():
        rel = [key for key in pluralToSingle if pluralToSingle[key] == rel]
        rel = rel[0]
    elif isMany:
        rel += 's'
    
    quote = f'The {rel} of {person} {"is" if not isMany else "are"} '
    
    if not isMany:
        quote += results[0] + "."
    else:
        for i in range(len(results)):
            if (i+1 == len(results)):
                quote += f" and {results[i]}."
            else:
                quote += f"{results[i]}"

            if(i < len(results) - 2):
                quote += ", "

    printBotHeader(quote)
    
def combineChildren(children):
    childrenList = ' '.join(children[0].split(',')).split()

    childrenList.append(children[1])
    
    childrenList = "['" + '\',\''.join(childrenList) + "']"

    return childrenList

def findRelationship(sentence):
    foundRel = None

    for rel in singular_rel:
        if re.search(r'\b%s' %rel, sentence):
            foundRel = rel

    return foundRel

def findPattern(sentence, patternList):
    for pattern in patternList:
        matched = re.match(pattern, sentence)

        # Since we found the relationship, we return it along with the matched pattern.
        if matched:
            rel = findRelationship(sentence)
            return matched, rel

    # Even if we matched it to a pattern, no relationship was found meaning it was invalid.
    return None, None

def constructResult(pattern, rel, promptType):
    query = ""
    parameters = list(pattern.groups())
    initTables()

    if(len(parameters) > 2):
        parameters[0] = combineChildren(parameters)
        del parameters[1]
        convertedParams = parameters
        convertedParams[1] = f"\'{convertedParams[1]}\'"
    else:
        convertedParams = ["'{}'".format(i) for i in parameters]

    match promptType:
        case "Boolean":
            query = f"{rel}({convertedParams[0]}, {convertedParams[1]})."
            # print(f"[QUERY] {query}")

            try:
                results = bool(list(prolog.query(query)))
                # print(results)
                printBotHeader("Yes! You're absolutely right." if results else "No, that's not quite right.")

            except Exception as e:
                print(f"Error: {e}")

        case "Who":
            if rel in pluralToSingle.keys():
                rel = pluralToSingle[rel]
                
            query = f"{rel}(X,{convertedParams[0]})."
            print(f"[QUERY] {query}")

            try:
                results = list(prolog.query(query))
                # print(f"Raw Results: {results}")
                resultList = []

                if not bool(results):
                    printBotHeader("I currently don't know that.")
                else:
                    for result in results:
                        resultList.append(result['X'])
                    printResults(resultList, parameters[-1], rel)


            except Exception as e:
                print(f"Error: {e}")
            
        case "Insert":
            # Query 1 is to check whether that relationship already exists.
            # Query 2 is to check if that relationship can exist.
            query1 = f"{rel}({convertedParams[0]}, {convertedParams[1]})."
            query2 = f"infer({rel}, {convertedParams[0]}, {convertedParams[1]})."
            # print(f"[QUERY] {query1}")
            # print(f"[QUERY] {query2}")
            
            try:
                results = bool(list(prolog.query(query1)))
                
                if(results):
                    printBotHeader("Thanks for telling me, but I already knew that.")
                else:
                    results = bool(list(prolog.query(query2)))
                    printBotHeader("OK! I learned something." if results else "That's impossible!")

            except Exception as e:
                print(f"Error: {e}")

        case _:
            return None

def parseSentence(sentence):
    if sentence == "":
        print("Invalid Prompt, try again.")
        return

    pattern, rel = None, None
    # Get the first word of the sentence
    match sentence.split()[0]:
        case "Is" | "Are":
            promptType = "Boolean"
            pattern, rel = findPattern(sentence, yesNoQuestions)
        case "Who":
            promptType = "Who"
            pattern, rel = findPattern(sentence, whoQuestions)
        case _:
            promptType = "Insert"
            pattern, rel = findPattern(sentence, factStatements)

    if pattern and rel:
        # print(pattern, pattern.groups())
        constructResult(pattern, rel, promptType)
        return
    
    print("Invalid Prompt, try again.")

prolog = Prolog()

os.system('cls')
connected = bool(list(prolog.query("[\'Prolog/newerKB.pl\', \'Prolog/assertions.pl\']")))
print(c.BOLD + "[PROLOG]" + c.END +" Successfully Connected to Knowledge Base!" if connected
      else "[PROLOG] Failed to Connect to Knowledge Base!")

print('#'*50)
print((" "*48).join('#'*2))
print(f"#    Hi there, welcome to our {c.BOLD}{c.CYAN}CHATBOT SYSTEM{c.END}.    #")
print((" "*48).join('#'*2))
print('#'*50)
print("To get started, input 'help' to view the commands!")

while True:
    # DEBUGGING
    
    choice = input(f"\n$ {c.CYAN}Prompt{c.END} > ")

    match choice.lower():
        case "help":
            print("Type any of the valid prompts and watch chatbot answer!\n")
            print(f"{c.BOLD}[COMMANDS]{c.END}")
            print("[ HELP ] - View commands and instructions.")
            print("[ SAVE ] - Save the knowledge base.")
            print("[DELETE] - Delete the knowledge base.")
            print("[ EXIT ] - Exit the program.")

        case "delete":
            result = bool(list(Prolog.query("delete_all.")))
            print("Successfully deleted the knowledge base!")

        case "exit":
            print("Exiting the program...")
            break

        case "save":
            result = bool(list(Prolog.query("save_all.")))
            print("Successfully saved knowledge base!" if result else "Something went wrong while saving knowledge base.")

        case "debug":
            print(f"{c.RED}{c.BOLD}[ENTERING DEBUG MODE]{c.END}")
            print("Type 'exit' to exit debugging mode.")

            while True:
                choice = input(f"\n$ {c.CYAN}Prompt{c.END} > ")

                if choice.lower() == "exit":
                    break
            
                results = list(prolog.query(choice))
                print(f"Raw Results: {results}")
                print(bool(results))
            
            print("Exited debug mode.")
        case _:
            parseSentence(choice)
