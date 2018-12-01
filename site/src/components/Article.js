import React from 'react';

import articles from "../data/articles.json"
import PatreonButton from './PatreonButton'

export default ({match}) => {
    const article = articles.find(({slug}) => slug === match.params.slug)
    if (!article) {
        return <div> couldn't find: {match.params.slug}</div>
    }
    return <article className="section content"> 
        <div dangerouslySetInnerHTML={{__html: article.content}}>
        </div>
        <br/>
        <p>Hopefully you learned something! If so, please consider supporting more posts like this by pledging on my <a href="https://www.patreon.com/bePatron?u=7263362">Patreon page</a>! It takes quite a bit of work to put these things together, if I saved you some time consider sending a few bucks my way for a coffee or two!</p>
        <div className="centered"> <PatreonButton /></div>
    </article>
}
