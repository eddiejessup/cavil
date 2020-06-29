import React, { useState, useEffect } from 'react';
import './App.css';

function App() {
  const [error, setError] = useState<Error | null>(null);
  const [isLoaded, setIsLoaded] = useState<boolean>(false);
  const [decisionToken, setDecisionToken] = useState<string | null>(null)

  useEffect(() => {
    const fetchData = async () => {
      setIsLoaded(false)
      setError(null)
      // try {
        const res = await fetch("/case/bar")
        console.log(res)
        const resJson = await res.json()
        console.log(resJson)
        console.log(resJson.decisionToken)
        setDecisionToken(resJson.decisionToken)
      // } catch (error) {
      //   setError(error)
      // }
      setIsLoaded(true);
    }
    fetchData()
  }, [])

  let msg
  if (error) {
      msg = <p>Error: {error.message}</p>;
  } else if (!isLoaded) {
    msg = <p>Loading...</p>;
  } else if (decisionToken) {
    msg = <p>Decision token: {decisionToken}</p>
  } else {
    msg = <p>No decision token found</p>
  }

  return (
    <div className="App">
      <header className="App-header">

        {msg}
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
    </div>
  );
}

export default App;
