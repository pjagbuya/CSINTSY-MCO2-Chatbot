from pyswip import Prolog


prolog = Prolog()
file_dir_to_PL = "../finalTestMP.pl"
prolog.consult(file_dir_to_PL)


userIn = ""

print("Hi there, welcome to our Chatbot System (Enter 'X' to exit program)")
while(True):
    print(">", end=" ")
    userIn = input()
    if userIn == "X":
        break
    user_input_escaped = userIn.replace("'", "\\'")

    query = f"assess_input('{user_input_escaped}', Response)."
    print("Query sent to Prolog: ", query)

    results = list(prolog.query(query))
    if results:

        # Geting response
        for result in results:
            # Only gets the Response variable
            response = result["Response"]

            # Chance of response being a list
            if isinstance(response, list):
                count = 0
                amt = len(response)
                for item in response:
                    print(f"{item.decode('utf-8').title()}", end="")
                    count += 1
                    if count != amt:
                        print(", ", end="")
                    elif count == amt:
                        print(".", end="")
                        print("")
                break
            # Sometimes its a yes or impossible output
            elif isinstance(response, bytes):
                response = response.decode('utf-8')
            print(response)
    else:
        # For debug purposes
        print("No response from Prolog.")



print("Good Bye! User.")



