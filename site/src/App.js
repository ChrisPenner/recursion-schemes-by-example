import React  from 'react';
import { BrowserRouter as Router, Route } from "react-router-dom"

import './App.css';
import './style/syntax.css';
import NavBar from "./components/NavBar"
import Contents from './components/Contents'
import Article from './components/Article'

const App = () => (
  <Router>
    <div>
      <NavBar />
      <Route exact path="/" component={Contents} />
      <Route exact path="/articles/:section/:slug" component={Article} />
    </div>
  </Router>
);

export default App;


