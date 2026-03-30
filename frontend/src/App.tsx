import { useEffect, useRef, useState } from 'react'
import reactLogo from './assets/react.svg'
import viteLogo from './assets/vite.svg'
import heroImg from './assets/hero.png'
import './App.css'

function App() {
  const wsRef = useRef<WebSocket | null>(null);

  const [message, setMessage] = useState("");


  

  useEffect(() => {
    const ws = new WebSocket("ws://localhost:9160");
    wsRef.current = ws;

    ws.onopen = () => {
      console.log("Connected!");
    };

    ws.onmessage = (event) => {
      console.log("Från server:", event.data);
    };

    ws.onclose = () => {
      console.log("closed");
    };

    ws.onerror = (err) => {
      console.error(err);
    };

    return () => ws.close();
  }, []);

  const sendMessage = () => {
    if (wsRef.current && wsRef.current.readyState === WebSocket.OPEN) {
      wsRef.current.send(message);
     
    } else {
      console.log("WebSocket inte redo");
    }
  };

  return (
    <div>
      <div>Se konsollen</div>
      <textarea
        value={message}
        onChange={(e) => setMessage(e.target.value)} />
      <button onClick={sendMessage}>Skicka meddelande</button>
    </div>
  );
}

export default App
