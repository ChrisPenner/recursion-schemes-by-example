import React from 'react';
import {Link} from 'react-router-dom'

import MailingListForm from './MailingListForm'
import articles from "../data/articles.json"
import groupBy from "lodash.groupby"
import ReactHover from 'react-hover'

const sortedArticles = groupBy(articles, a => a.section)
const options = {
  followCursor:false,
  shiftX: 0,
  shiftY: 0
}

const HoverDescription = ({what, why}) => <div className="hover-card">
    <div><b>{what}</b></div>
    <p>{why}</p>
</div>

 
const buildArticleLink =
    ({title, url, what, why}) =>
    <ReactHover
        key={url}
        options={options}>
        <ReactHover.Trigger type='trigger'>
            <li key={url}><Link to={url}>{ title }</Link></li>
        </ReactHover.Trigger>
        <ReactHover.Hover type='hover'>
            <HoverDescription what={what} why={why} />
        </ReactHover.Hover>
    </ReactHover>

const buildSection = ([section, as]) => 
    <ol key={section} type="1">
        <li> {section}
            <ol type="i"> 
                {as.map(buildArticleLink)}
            </ol>
        </li>
    </ol>

export default () => {
    const articleList = Object.entries(sortedArticles).map(buildSection)
    return <div>
        <section className="section">
            <h1 className="subtitle">Sections</h1>
            <div className="content">
                {articleList}
            </div>
        </section>
        <MailingListForm />
    </div>
}
