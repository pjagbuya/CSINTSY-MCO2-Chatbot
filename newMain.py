import re

from pyswip import Prolog
prolog = Prolog()
results = bool(list(prolog.query("[\'newKB.pl\', \'assertions.pl\']")))
print("[PROLOG] Successfully Connected to Knowledge Base!" if results else "[PROLOG] Failed to Connect to Knowledge Base!")
# prolog.consult("[\'newKB.pl\', \'saved.pl\']")

#for sentence pattern relationship
singular_rel = ["sibling", "brother", "sister",
                "father", "mother", "parent",
                "grandmother", "grandfather", "child",
                "daughter", "son", "uncle", "aunt" ]

# Regex Pattern for Questions
yesNoQuestions = [  r'Are (.+) and (.+) siblings\?',                r'Is (.+) a sister of (.+)\?',
                    r'Is (.+) a brother of (.+)\?',                 r'Is (.+) the mother of (.+)\?',
                    r'Is (.+) the father of (.+)\?',                r'Are (.+) and (.+) the parents of (.+)\?',
                    r'Is (.+) a grandmother of (.+)\?',             r'Is (.+) a daughter of (.+)\?',
                    r'Is (.+) a son of (.+)\?',                     r'Is (.+) a child of (.+)\?',
                    r'Are (.+) and (.+) children of (.+)\?',        r'Is (.+) an uncle of (.+)\?',
                    r'Is (.+) an aunt of (.+)\?',                   r'Are (.+) and (.+) relatives\?',
                    r'Is (.+) grandfather of (.+)\?']

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

def infer(sentence):
    pass

def findRelationship(sentence):
    foundRel = None
    
    # # First check plural relationships since they are a priority.
    # for rel in plural_rel:
    #     findRelationship = re.search(r"\b%s" %rel, sentence)

    #     if findRelationship:
    #         foundRel = rel

    # Check singular relationships after.
    if foundRel is None:
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
    parameters = pattern.groups()

    match promptType:
        case "Boolean":
            query = rel + "(\'" + '\',\''.join(parameters) + "\')."
            print(f"[QUERY] {query}")

            try:
                results = bool(list(prolog.query(query)))
                print(results)
            except Exception as e:
                print(f"Error: {e}")

        case "Who":
            query = f"{rel}(X,\'{parameters[0]}\')."
            print(f"[QUERY] {query}")

            try:
                results = list(prolog.query(query))
                print(f"Raw Results: {results}")

                for result in results:
                    print(result['X'])

            except Exception as e:
                print(f"Error: {e}")
            
        case "Insert":
            query = "infer(" + rel + ",\'" + "\',\'".join(parameters) + "\')."
            print(f"[QUERY] {query}")

            try:
                results = bool(list(prolog.query(query)))
                print(results)
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
        case "save_all":
            result = bool(list(Prolog.query("save_all.")))
            print("Saved all.", result)
            return
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
        print(pattern, pattern.groups())
        constructResult(pattern, rel, promptType)
        return
    
    print("Invalid Prompt, try again.")

while True:
    print("\n-------------------------\n    WELCOME TO THE CHATBOT!\n-------------------------")
    
    # DEBUGGING
    sentence = "Is bro a brother of alex?"
    
    choice = input("> ")
    # results = bool(list(prolog.query(choice)))
    # print(results)
    
    parseSentence(choice)